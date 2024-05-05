### emacs copilot using gptscript

Justine Tunney has a nice little [emacs copilot package](https://github.com/jart/emacs-copilot). It uses llamafiles to run local LLMs to serve the code completion requests. However it is tightly integrated with llamafiles, has its own cache and history facilities, it is not very easy to extend it to use other LLMs, either local or remote.

[GPTScript](https://github.com/gptscript-ai/gptscript) has a great plugin architecture, enables using tools from github repos, and connecting to either local LLMs (such as ollama, llamafiles), or remote LLMs (mistral, anthropic, fireworks, etc.), as long as they support OpenAI compatible api or there are providers (or "shim") which does the api translation. GPTScript has [open source providers for major cloud vendors](https://docs.gptscript.ai/alternative-model-providers).

So this repo is an attempt to adapt Justine's nice little package to use GPTScript to connect to local/remote LLMs. It uses GPTScript chat mode (chat state) to transfer the code completion history/context to LLMs, so that LLMs can generate new code based on previous code. When you delete("kill") code from emacs editor, it will also be deleted from code completion context (chat state) when next time you request LLMs for new code.

1. Installation:
   * first [install GPTScript as instructed](https://github.com/gptscript-ai/gptscript).
   * git clone https://yglcode/emacs-copilot to <code>INSTALL_DIR</code>
   * ```add $INSTALL_DIR/bin to your PATH; chmod a+x $INSTALL_DIR/bin/*```
   * add following code to your ~/.emacs file:
     ```elisp
     (setq load-path (cons 
		 "...path to $INSTALL_DIR.../gptscript-copilot" 
		 load-path))
     (require 'copilot)
     ```
   * existing key bindigs support C, python, go, java; you can add key bindings for your language similar to following:
     ```elisp
     (defun copilot-java-hook ()
        (define-key java-mode-map (kbd "C-c C-k") 'copilot-complete))
     ```

2. Connect LLMs:
   emacs copilot uses env var LLMODEL to point to (or change) LLM to use. You need to set up LLM api keys (and its env vars) and LLMODEL before starting emacs and run copilot.
   
   ##### _note: emacs copilot works best with LLMs from commercial vendors, many times responses from local/small LLMs cause misbehaviours._ #####
   
   GPTScript support connecting to local and remote LLMs. Please consult [the instructions](https://docs.gptscript.ai/alternative-model-providers).
   * remote LLMs:
     * for most LLMs vendors, you need their API keys (and credits for your test)
     * set up api keys env vars as instructed, such as 
       * mistral: GPTSCRIPT_PROVIDER_API_MISTRAL_AI_API_KEY
     * set LLMODEL to point to your target LLMs, such as
       * OpenAI: 
         * export LLMODEL="gpt-3.5-turbo-0125" 
         * export LLMODEL="gpt-4-turbo-preview"
       * fireworks: 
         * export LLMODEL="accounts/fireworks/models/llama-v3-8b-instruct from https://api.fireworks.ai/inference/v1"
   * local LLMs:
     * ollamas: 
       * [install ollama](https://ollama.com/) and pull a model such as llama3
       * run "ollama serve"
       * choose model: export LLMODEL="llama3:latest from http://localhost:11434/v1"
     * llamafile:
       * [download llamafiles](https://huggingface.co/jartine)
       * [make it executable and run it](https://github.com/Mozilla-Ocho/llamafile)
       * point to it (if you run mistral7b):
         export LLMODEL="mistral-7b-instruct-v0.2.Q4_K_M.gguf from http://127.0.0.1:8080/v1"
         
3. Usage:
   * place your cursor at end of line or below the line you need complete,
       this line could be a comment, a function signature or a type name,
       currently you can capture only 2 lines of info.
   * use `C-c C-k' to start completion process and `C-g' to stop it.
   * copilot (or LLMs) is language neutral, it uses file extension to identify the language to use.

4. Issues:
   * local/small LLMs perform poorly compared to large/commercial LLMs
