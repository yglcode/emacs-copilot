### emacs copilot using gptscript

Justine Tunney has a nice little [emacs copilot package](https://github.com/jart/emacs-copilot). It uses llamafiles to run local LLMs to serve the code completion requests. However it is tightly integrated with llamafiles, has its own cache and history facilities, it is not very easy to extend it to use other LLMs, either local or remote.

[GPTScript](https://github.com/gptscript-ai/gptscript) has a great plugin architecture, enables using tools from github repos, and connecting to either local LLMs (such as ollama, llamafiles), or remote LLMs (mistral, anthropic, fireworks, etc.), as long as they support OpenAI compatible api or there are providers (or "shim") which does the api translation. GPTScript has [open source providers for major cloud vendors](https://docs.gptscript.ai/alternative-model-providers).

So this repo is an attempt to adapt Justine's nice little package to use GPTScript to connect to local/remote LLMs. It uses GPTScript chat mode (chat state) to transfer the code completion history/context to LLMs, so that LLMs can generate new code based on previous code. When you delete("kill") code from emacs editor, it will also be deleted from code completion context (chat state) when next time you request LLMs for new code.

1. Installation:
   * first [install GPTScript as instructed](https://github.com/gptscript-ai/gptscript).
   * make sure ```jq``` installed
   * ```git clone https://yglcode/emacs-copilot to INSTALL_DIR```
   * ```add $INSTALL_DIR/bin to your PATH; chmod a+x $INSTALL_DIR/bin/*```
   * add following code to your ~/.emacs file:
     ```elisp
     (setq load-path (cons 
		 "...path to $INSTALL_DIR.../gptscript-copilot" 
		 load-path))
     (require 'copilot)
     ```
   * existing key bindings support C, python, go, java; you can add key bindings for your language similar to following:
     ```elisp
     (defun copilot-java-hook ()
        (define-key java-mode-map (kbd "C-c C-k") 'copilot-complete))
     ```

2. Connect LLMs:
   
   This emacs copilot uses env var ```$LLMODEL``` to point to (or change) LLM to use. You need to set up LLM api keys (and its env vars, shown below) and ```$LLMODEL``` before starting emacs and run copilot.
   
   #### _note: emacs copilot works best with LLMs from commercial vendors, many times responses from local/small LLMs cause misbehaviours._ ####
   
   GPTScript support connecting to local and remote LLMs. Please consult [the instructions](https://docs.gptscript.ai/alternative-model-providers).
   * remote LLMs:
     * for most LLMs vendors, you need their API keys (and credits for your test)
     * set up api keys env vars as instructed, such as 
       * mistral:
       
            ```export GPTSCRIPT_PROVIDER_API_MISTRAL_AI_API_KEY=<your mistral api key>```
     * set LLMODEL to point to your target LLMs, such as
       * OpenAI: 
         * ```export LLMODEL="gpt-3.5-turbo-0125"``` 
         * ```export LLMODEL="gpt-4-turbo-preview"```
       * fireworks: 
         * ```export LLMODEL="accounts/fireworks/models/llama-v3-8b-instruct from https://api.fireworks.ai/inference/v1"```
   * local LLMs:
     * ollama: 
       * [install ollama](https://ollama.com/) and pull a model such as llama3
       * run "ollama serve"
       * choose model:
           ```export LLMODEL="llama3:latest from http://localhost:11434/v1"```
     * llamafile:
       * [download llamafiles](https://huggingface.co/jartine)
       * [make it executable and run it](https://github.com/Mozilla-Ocho/llamafile)
       * point to it: assuming you run mistral7b
           ```export LLMODEL="mistral-7b-instruct-v0.2.Q4_K_M.gguf from http://127.0.0.1:8080/v1"```

3. Copilot as coder (code completion):
   
    select a code or comment block (as prompt request) and start completion process.

   * actions to start code completion
      * use ```C-c C-k``` (or elisp function copilot-complete()) to start completion process and ```C-g``` to stop it.
      * use ```C-c C-x C-k``` (or elisp function copilot-reset()) to clear copilot code-completion history, if you need a coding memory reset for brand new tasks.
      * copilot (or LLMs) is language neutral, it uses file extension to identify the language to use.
  
   * how to select target text (code or documents or comments) block:
      * the target block can be a single line or many continuous lines of code or comment, by simply place your cursor at end of or below it, you designate it as the completion target (emacs copilot will search backwards until a empty line). this could be a comment requesting some code, a beginning (incomplete) part of code such as a function or a type definition:
        ```go
        func bubble_sort(data []int) {
        ......
        //use above defined Node and Edge types, define a Graph type
        ```
      * you can also use emacs [region selection](https://www.gnu.org/software/emacs/manual/html_node/emacs/Mark.html) to select a whole region of code and comments (including many types and function definitions) to send to LLM as prompt. You can even select whole file content. Of course the last (few) lines of selected region shoule be incomplete code or comments requesting for specific code generation.

   * use documents and comments as generic prompting facility for code transformation and generation.
  
      you can add documents or comment lines at end of a target range of code (types and functions) to prompt LLM for desired code changes and generation, then select this range including the last comment/prompt lines and start  completion process. LLM will generate new code with transformations you requested.
     * add docs and comments
        ```go
        type Graph struct {
          Nodes []*Node
          Edges []*Edge
        }
        //add documents and comments to above types and their fields
        ```
      * add tags to types' fields for json serialization
        ```go
        type Graph struct { 
          ... 
        }
        //add tags for above types for json serialization
        ```
      * add unit tests
        ```go
        func bubble_sort(data []int) {
          ...
        }
        func quick_sort(data []int) {
          ...
        }
        //add table driven unit tests for above functions
        ```
4. Copilot as expert/advisor (code review and advice):
   * actions to start expert conversation
      * use ```C-c C-e``` (or elisp function copilot-expert()) to start expert conversation and ```C-g``` to stop it.
      * use ```C-c C-x C-e``` (or elisp function copilot-reset()) to clear copilot expert conversation history.
   * use documents and comments as generic prompting for copilot expert. 
     * Similar to the cases of "Copilot as coder", you can add comments under target code block to ask expert to review code, find bug, or recommend improvement
     
        ```go
        type ... {...}
        func ... {...}
        //review above type, tell me what is it for
        //review above type, can you find any bug
        //given above code, how to make it faster
        ```
     * You can also ask general CS questions
     
        ```code
        //teach me about dijkstra algorithm
        ```
5. Copilot as robin (for funny talk):
   
    use plain text or comments as prompt for robin.
   
    * use ```C-c C-y``` to start talking to robin
    * use ```C-c C-x C-y``` to reset conversation

6. Issues:
   * local/small LLMs perform poorly compared to large/commercial LLMs
