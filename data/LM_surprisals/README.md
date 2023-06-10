# LM-surprisals

Directory contains surprisal estimates from LMs.

## LMs

Huggingface LMs we use (see info [here](https://github.com/huggingface/transformers/blob/6c08840628a22a4d53ae563d1041479649d1a8e7/docs/source/pretrained_models.rst))

- **GPT**	

  max context length: 512
    - `openai-gpt`:

      12-layer, 768-hidden, 12-heads, 110M parameters.
      OpenAI GPT English model

- **GPT2**

  max context length: 1024
  - `gpt2`:

      12-layer, 768-hidden, 12-heads, 124M parameters (not 117).
      Original release of OpenAI GPT-2 English model

  - `gpt2-large`:

      36-layer, 1280-hidden, 20-heads, 774M parameters.
      OpenAI's Large-sized GPT-2 English model

  - `gpt2-xl`:

      48-layer, 1600-hidden, 25-heads, 1558M parameters.
      OpenAI's final-release XL-sized GPT-2 English model

- **GPT-Neo**

  max context length: 2048

  - `EleutherAI/gpt-neo-2.7B`:

      32-layer, 2560-hidden, 20-heads, 2.7B parameters.
      EleutherAI's GPT-3 like language model.

- **GPT-J**

  max context length: 2048

  - `EleutherAI/gpt-j-6B`:

      28-layer, 4096-hidden, 16-heads, 6B parameters.
      EleutherAI's newer GPT-3 'Curie'-like language model.

Also, GPT3 models from OpenAI:

- **GPT3**

  max context length: 2048

  - "Ada"

  - "Curie"

  - "da Vinci":

      175G parameters
