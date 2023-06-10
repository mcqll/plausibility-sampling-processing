import os
import openai
import pandas as pd
from tqdm import tqdm
import argparse

# Load your API key from environment variable
openai.api_key = os.getenv("OPENAI_API_KEY")

from transformers import GPT2TokenizerFast


class OpenAiLM:
    """
    Class to use transformers model for causal LM to get surprisal estimates.
    """
    def __init__(self, engine, maxlen=None):
        self.engine = engine
        self.maxlen = maxlen
        self.tokenizer = GPT2TokenizerFast.from_pretrained("gpt2")
        
    def gptfixinit(self, string):
        tokenizer = self.tokenizer
        # if the word is split into a different number of pieces at beginning of string than
        # it would be elsewhere, then use the elsewhere tokenization.
        firstword = string.split(" ")[0]
        if len(tokenizer(firstword).input_ids) != len(tokenizer(" "+firstword).input_ids):
            return ' ' + string
        else: return string

    def string_to_surprisals(self, input_string, string_id=None):
        input_string = self.gptfixinit(input_string)
        response = openai.Completion.create(
            engine=self.engine, 
            prompt=input_string, 
            echo=True,     # get info about input
            max_tokens=0,  # no predictions past string end
            logprobs=0     # get log probs of input and top 0 others
            )
        output = response.choices[0].logprobs
        surprisals = [-1 * q if q is not None else q for q in output.token_logprobs]
        return {
            "token": output.tokens,
            "offset": output.text_offset,
            "surprisal": surprisals,
            "openai_engine": self.engine,
            "model": response.model,
            "id": response.id,
            "created": response.created,
            "string_id": string_id
        }

    def get_naturalstories_surprisals(self, data):
        """ 
        data: list of strings (default, the 10 natural stories texts)
        """
        df = pd.DataFrame()
        print(f'getting {self.engine} surprisals on each story')
        for i, story in tqdm(enumerate(data), desc="story"):
            df=df.append(
                pd.DataFrame(self.string_to_surprisals(story, string_id=i+1)),
                ignore_index=True)
        return df
    
    def get_naturalstories_surprisals_bysentence(self, data):
        """
        data: list of lists of strings (default: 10 lists of ~1000 sentences each)
        """
        df = pd.DataFrame()
        print(f'getting {self.engine} surprisals on each story, by sentence')
        for i, sentencelist in tqdm(enumerate(data), desc="story"):
            for j, sent in tqdm(enumerate(sentencelist), desc="sentence", leave=False):
                df=df.append(
                    pd.DataFrame(
                        self.string_to_surprisals(sent, string_id=f'{i+1}_{j+1}')),
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
        modelname, engine, maxlen, name_prefix="",
        data=naturalstories_storylist):
    m = OpenAiLM(engine=engine, maxlen=maxlen)
    df = m.get_naturalstories_surprisals(data)
    out_string = f"../data/LM_surprisals/naturalstories_surprisals_{name_prefix}{modelname}.csv"
    print(f"Saving surprisals from {modelname} to {out_string}")
    df.to_csv(out_string, index=None)
    return 0

def write_ns_surps_bysent(
        modelname, engine, maxlen, name_prefix="",
        data=naturalstories_storylist_sentlist):
    m = OpenAiLM(engine=engine, maxlen=maxlen)
    df = m.get_naturalstories_surprisals_bysentence(data)
    out_string = f"../data/LM_surprisals/naturalstories_surprisals_{name_prefix}{modelname}_bysent.csv"
    print(f"Saving surprisals from {modelname} to {out_string}")
    df.to_csv(out_string, index=None)
    return 0


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Get surprisals from OpenAI API")
    # parser.add_argument('-a', '--get_all_default',
    #     action='store_true', help="Get surprisals from all default models.")
    parser.add_argument('-s', '--get_specific',
        action='store_true', help="Run specific models (modify code as needed).")
    args = parser.parse_args()

    if args.get_specific:
        # write_ns_surps("GPT3-ada", "ada", maxlen=None)
        # write_ns_surps("GPT3-babbage", "babbage", maxlen=None)
        write_ns_surps("GPT3-curie", "curie", maxlen=None)
        # write_ns_surps("GPT3-davinci", "davinci", maxlen=None)

        # write_ns_surps_bysent("GPT3-ada", "ada", maxlen=None)
        # write_ns_surps_bysent("GPT3-babbage", "babbage", maxlen=None)
        write_ns_surps_bysent("GPT3-curie", "curie", maxlen=None)
        # write_ns_surps_bysent("GPT3-davinci", "davinci", maxlen=None)