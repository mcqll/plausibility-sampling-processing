import torch
import numpy as np
import pandas as pd
import itertools
import re
from tqdm.notebook import tqdm
import multiprocessing
from joblib import Parallel, delayed
import argparse

# Set the cache directory to somewhere 
# where you have space for multiple ~20GB models
# before importing transformers
import os
os.environ['TRANSFORMERS_CACHE'] = '/datadrive/surprisals/.cache/'
from transformers import AutoTokenizer, AutoModelForCausalLM

class LM:
    """
    Class to use transformers model for causal LM to get surprisal estimates.
    """
    def __init__(
            self, model_spec,
            device=torch.device('cpu'), 
            maxlen=None, **kwargs):

        print("Initializing... ")
        self.device = device
        self.model_spec = model_spec
        self.model = AutoModelForCausalLM.from_pretrained(
            pretrained_model_name_or_path=model_spec, **kwargs).to(device)
        self.tokenizer = AutoTokenizer.from_pretrained(model_spec)
        if maxlen!=None:
            if type(maxlen)==int:
                self.max_context_length = maxlen 
            if type(maxlen)==str:
                config = self.model.config.get_config_dict(model_spec)[0]
                self.max_context_length = config[maxlen]
        else:
            self.max_context_length=self.model.config.n_positions
        print(f"\nLanguage model '{model_spec}' " +
              f"loaded on {device}.\nUsing max context length = {self.max_context_length}")

    def string_to_surprisals(self, input_string, string_id=None):
        if self.model_spec=='transfo-xl-wt103': # fix only apparent issue in txl tokenizer
            input_string=re.sub("^'","' ",input_string)
        else: # otherwise (for GPT models) just add a space before the first word.
            def gptfixinit(s):
                # if the word is split into a different number of pieces at beginning of string than
                # it would be elsewhere, then use the elsewhere tokenization.
                firstword = s.split(" ")[0]
                if len(self.tokenizer(firstword).input_ids) != len(self.tokenizer(" "+firstword).input_ids):
                    return ' ' + s
                else: return s
            input_string=gptfixinit(input_string)
        input_ids = self.tokenizer(input_string, return_tensors="pt").input_ids
        input_len = len(input_ids[0])
        if input_len <= self.max_context_length:
            # just run once, if the story is short enough
            surprisals = self._ids_to_surprisals(input_ids)

        else:
            first_input_ids = input_ids[:,:self.max_context_length]
            first_surprisals = self._ids_to_surprisals(first_input_ids)
            surprisals = first_surprisals
            length_difference=input_len-self.max_context_length
            print(f"String length is {input_len}, which is "
                  f"longer than max length {self.max_context_length}.\n"
                  f"Will have to run model {length_difference} times.")
            for i in tqdm(range(1,length_difference+1), desc=f"{self.model_spec}"):
                #for each token after the max length, run
                input_ids_from_i_on = input_ids[:,i:i + self.max_context_length]
                surprisals_from_i_on = self._ids_to_surprisals(input_ids_from_i_on)
                surprisal_last_starting_at_i = surprisals_from_i_on[-1]
                surprisals+=[surprisal_last_starting_at_i]

        raw_tokens = self.tokenizer.convert_ids_to_tokens(input_ids[0])
        string_tokens = [self._stringify(t) for t in raw_tokens]
        len_tokens = [0] + [len(t) for t in string_tokens]
        offset=list(itertools.accumulate(len_tokens))[:-1]
        return {
            "token": string_tokens,
            "offset": offset,
            "surprisal": surprisals,
            "model": self.model_spec,
            "string_id": string_id
        }
    
    def _stringify(self, raw_token):
        if self.model_spec=="transfo-xl-wt103":
            return raw_token
        else:
            return self.tokenizer.convert_tokens_to_string(raw_token)

    def _ids_to_surprisals(self, input_ids):
        # check that the input ids list isn't overlong
        assert(len(input_ids) <= self.max_context_length)
        with torch.no_grad():
            outputs = self.model(
                input_ids=input_ids.to(self.device), 
                # labels=input_ids.to(self.device)
            )
        # loss = outputs.loss
        # past_key_values = outputs.past_key_values
        logits = outputs.logits.cpu()
        logprobdists = torch.log_softmax(logits, dim=-1) # [batch_size, input_ids_len, vocab_size]
        # surprisal value is from previous word output 
        # (surprisal of word i is from logits at position i-1)
        # first word has undefined surprisal
        predict_at = range(len(input_ids[0])-1)
        surprisals = [np.nan] + list( 
            -logprobdists[0, predict_at, input_ids[0][1:]].numpy())
        return surprisals

    def get_naturalstories_surprisals(self, data):
        """ 
        data: list of strings (default, the 10 natural stories texts)
        """
        df = pd.DataFrame()
        for i, story in tqdm(
            enumerate(data), total=len(data),
            desc=f"{self.model_spec} surprisals, by text"):
            df=df.append(
                pd.DataFrame(self.string_to_surprisals(story, string_id=i+1)),
                ignore_index=True)
        return df

    def get_naturalstories_surprisals_parallel(self, data, num_cores):
        """ 
        data: list of strings (default, the 10 natural stories texts)
        """
        df = pd.DataFrame()

        indices = list(range(len(data)))

        def get_surps(story,i):
            print(f'getting {self.model_spec} surprisals on story {i+1}')
            return(self.string_to_surprisals(story, string_id=i+1))

        surps = Parallel(n_jobs=num_cores)(
            delayed(get_surps)(data[i], i) for i in indices)

        for i in indices:
            df=df.append(
                pd.DataFrame(surps[i]),
                ignore_index=True)
        return df

    def get_naturalstories_surprisals_bysentence(self, data):
        """
        data: list of lists of strings (default: 10 lists of ~1000 sentences each)
        """
        df = pd.DataFrame()
        for i, sentencelist in tqdm(
            enumerate(data), total=len(data),
            desc=f"{self.model_spec} surprisals, by sent, by text"):
            for j, sent in enumerate(sentencelist):
                df=df.append(
                    pd.DataFrame(self.string_to_surprisals(sent, string_id=f'{i+1}_{j+1}')),
                    ignore_index=True)
        return df

naturalstories = pd.read_csv("../data/natural_stories_sentences.csv")
naturalstories_storylist = [ # a list of strings (one per story) 
    ' '.join(list(naturalstories.loc[(naturalstories["story_num"]==i)]['sentence']))
    for i in range(1,11)]
naturalstories_storylist_sentlist = [ # a list of lists (per story) of strings (per sent in story)
    list(naturalstories.loc[(naturalstories["story_num"]==i)]['sentence'])
    for i in range(1,11)] 

def write_ns_surps(
        modelname, model_spec, maxlen, name_prefix="",
        data=naturalstories_storylist):
    m = LM(device=torch.device('cpu'), model_spec=model_spec, maxlen=maxlen)
    df = m.get_naturalstories_surprisals(data)
    out_string = f"../data/LM_surprisals/naturalstories_surprisals_{name_prefix}{modelname}.csv"
    print(f"Saving surprisals from {modelname} to {out_string}")
    df.to_csv(out_string, index=None)
    return 0

def write_ns_surps_80(
        modelname, model_spec, maxlen, name_prefix="",
        data=naturalstories_storylist):
    m = LM(device=torch.device('cpu'), model_spec=model_spec, maxlen=maxlen)
    df = m.get_naturalstories_surprisals(data)
    out_string = f"../data/LM_surprisals/naturalstories_surprisals_{name_prefix}{modelname}_80.csv"
    print(f"Saving surprisals from {modelname} to {out_string}")
    df.to_csv(out_string, index=None)
    return 0

def write_ns_surps_bysent(
        modelname, model_spec, maxlen, name_prefix="",
        data=naturalstories_storylist_sentlist):
    m = LM(device=torch.device('cpu'), model_spec=model_spec, maxlen=maxlen)
    df = m.get_naturalstories_surprisals_bysentence(data)
    out_string = f"../data/LM_surprisals/naturalstories_surprisals_{name_prefix}{modelname}_bysent.csv"
    print(f"Saving surprisals from {modelname} to {out_string}")
    df.to_csv(out_string, index=None)
    return 0












if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Get surprisals from LMs")
    parser.add_argument('-a', '--get_all_default',
        action='store_true', help="Get surprisals from all default models.")
    parser.add_argument('-s', '--get_specific',
        action='store_true', help="Run specific models (modify code as needed).")
    # parser.add_argument('--get',
    #     action='store_true', help="")
    # parser.add_argument('--get_bysent',
    #     action='store_true', help="")
    # parser.add_argument('--get_bylen',
    #     action='store_true', help="")
    args = parser.parse_args()

    # # Set up what models to get surprisals from
    # # one tuple per set of arguments
    # modelname_list, model_spec_list, maxlen_list = zip(*[
    #     ('GPT2',       'gpt2',                    None), 
    #     ('GPT2-large', 'gpt2-large',              None),
    #     ('GPT-J',      'EleutherAI/gpt-j-6B',     None),
    #     ('GPT-Neo',    'EleutherAI/gpt-neo-2.7B', 'max_position_embeddings'),
    #     ('TXL',        'transfo-xl-wt103',        4000)
    # ])

    # modelname_list, model_spec_list, maxlen_list = zip(*[
    #     ('GPT2',       'gpt2',                    80), 
    #     # ('GPT2-large', 'gpt2-large',              None),
    #     ('GPT-J',      'EleutherAI/gpt-j-6B',     80),
    #     ('GPT-Neo',    'EleutherAI/gpt-neo-2.7B', 80)#,
    #     # ('TXL',        'transfo-xl-wt103',        4000)
    # ])

    # indices = list(range(len(modelname_list)))
    # num_cores_available = multiprocessing.cpu_count()
    # num_cores = num_cores_available # or I could set it to len(indices) if (len(indices) < num_cores_available) else num_cores_available

    # if args.get_all_default:
    #     print("TODO")

    # if args.get:
    # # Get surprisals by story (up to maxlen) in parallel
    #     Parallel(n_jobs=num_cores)(
    #         delayed(write_ns_surps)(
    #             modelname_list[i], model_spec_list[i], maxlen_list[i])
    #             for i in indices)
    #     print("Done all.")
    # if args.get_bysent:
    #     # Get surprisals by sentence in parallel
    #     Parallel(n_jobs=num_cores)(
    #         delayed(write_ns_surps_bysent)(
    #             modelname_list[i], model_spec_list[i], maxlen_list[i])
    #             for i in indices)
    #     print("Done all.")
    # if args.get_bylen:
    #     # Get surprisals by sentence in parallel
    #     Parallel(n_jobs=num_cores)(
    #         delayed(write_ns_surps_80)(
    #             modelname_list[i], model_spec_list[i], maxlen_list[i])
    #             for i in indices)
    #     print("Done all.")

    if args.get_specific:
        # GPT2-xl
        gpt2xl = LM(device=torch.device('cuda'), model_spec='gpt2-xl')
        gpt2xldf = gpt2xl.get_naturalstories_surprisals(naturalstories_storylist)
        gpt2xldf.to_csv("../data/LM_surprisals/naturalstories_surprisals_GPT2-xl.csv", index=None)
        gpt2xldf_bysent = gpt2xl.get_naturalstories_surprisals_bysentence(naturalstories_storylist_sentlist)
        gpt2xldf_bysent.to_csv("../data/LM_surprisals/naturalstories_surprisals_GPT2-xl_bysent.csv", index=None)
        gpt2xl = LM(device=torch.device('cuda'), model_spec='gpt2-xl', maxlen=80)
        gpt2xldf = gpt2xl.get_naturalstories_surprisals(naturalstories_storylist)
        gpt2xldf.to_csv("../data/LM_surprisals/naturalstories_surprisals_GPT2-xl_80.csv", index=None)
        # GPT1
        gpt1 = LM(device=torch.device('cuda'), model_spec='openai-gpt')
        gpt1df = gpt1.get_naturalstories_surprisals(naturalstories_storylist)
        gpt1df.to_csv("../data/LM_surprisals/naturalstories_surprisals_GPT1.csv", index=None)
        gpt1df_bysent = gpt1.get_naturalstories_surprisals_bysentence(naturalstories_storylist_sentlist)
        gpt1df_bysent.to_csv("../data/LM_surprisals/naturalstories_surprisals_GPT1_bysent.csv", index=None)
        gpt1 = LM(device=torch.device('cuda'), model_spec='openai-gpt', maxlen=80)
        gpt1df = gpt1.get_naturalstories_surprisals(naturalstories_storylist)
        gpt1df.to_csv("../data/LM_surprisals/naturalstories_surprisals_GPT1_80.csv", index=None)