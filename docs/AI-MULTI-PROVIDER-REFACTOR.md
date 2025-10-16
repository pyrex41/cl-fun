# AI Multi-Provider Refactor

**Date:** October 16, 2025
**Status:** Completed
**Scope:** AI agent system refactored to support multiple LLM providers (OpenAI, Groq, Anthropic)

## Overview

The AI agent system has been refactored from a Claude-only implementation to a multi-provider architecture supporting OpenAI, Groq, and Anthropic. The new implementation uses a LangChain-style configuration pattern with environment variables for easy provider switching.

## Motivation

- **Provider flexibility**: Ability to switch between different LLM providers based on cost, performance, or availability
- **Cost optimization**: Use different models for different workloads (e.g., Groq's fast inference for real-time features)
- **Redundancy**: Fall back to alternative providers if primary provider experiences issues
- **Model diversity**: Leverage different model strengths (OpenAI GPT-4o, Groq Llama, Claude Sonnet)

## Architecture Changes

### LangChain-Style Configuration

The system now uses environment variables for configuration, inspired by LangChain's approach:

```lisp
(defparameter *ai-provider* (or (uiop:getenv "AI_PROVIDER") "openai")
  "AI provider: openai, groq, or anthropic")

(defparameter *ai-model*
  (or (uiop:getenv "AI_MODEL")
      (cond
        ((string= *ai-provider* "openai") "gpt-4o-mini")
        ((string= *ai-provider* "groq") "llama-3.3-70b-versatile")
        ((string= *ai-provider* "anthropic") "claude-3-5-sonnet-20241022")
        (t "gpt-4o-mini")))
  "AI model to use for component generation")

(defparameter *ai-api-key*
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "GROQ_API_KEY")
      (uiop:getenv "ANTHROPIC_API_KEY"))
  "API key from environment variable")
```

### Provider Configuration Registry

Provider-specific details (base URLs, auth headers, models) are stored in a configuration registry:

```lisp
(defparameter *provider-config*
  '((:openai
     (:base-url "https://api.openai.com/v1/chat/completions")
     (:auth-header "Authorization")
     (:auth-format "Bearer ~A")
     (:models ("gpt-4o" "gpt-4o-mini" "gpt-4-turbo")))
    (:groq
     (:base-url "https://api.groq.com/openai/v1/chat/completions")
     (:auth-header "Authorization")
     (:auth-format "Bearer ~A")
     (:models ("llama-3.3-70b-versatile" "llama-3.1-70b-versatile" "mixtral-8x7b-32768")))
    (:anthropic
     (:base-url "https://api.anthropic.com/v1/messages")
     (:auth-header "x-api-key")
     (:auth-format "~A")
     (:models ("claude-3-5-sonnet-20241022" "claude-3-opus-20240229")))))
```

### Provider-Agnostic Interface

New `call-llm-api` function provides a unified interface:

```lisp
(defun call-llm-api (messages tools)
  "Call LLM API with provider-agnostic interface"
  (validate-api-key)

  (let* ((provider (intern (string-upcase *ai-provider*) :keyword))
         (config (get-provider-config provider)))

    (cond
      ;; OpenAI and Groq use OpenAI-compatible format
      ((or (eq provider :openai) (eq provider :groq))
       (call-openai-compatible-api messages tools config))

      ;; Anthropic uses different format
      ((eq provider :anthropic)
       (call-anthropic-api messages tools config))

      (t
       (error "Unsupported provider: ~A" provider)))))
```

### Response Normalization

Different providers return different response formats. The system normalizes them to a common format:

**OpenAI/Groq Response Format:**
```json
{
  "choices": [{
    "message": {
      "tool_calls": [{
        "function": {
          "name": "create_button",
          "arguments": "{\"x\": 100, \"y\": 200, \"text\": \"Click Me\"}"
        }
      }]
    }
  }]
}
```

**Anthropic Response Format:**
```json
{
  "content": [{
    "type": "tool_use",
    "name": "create_button",
    "input": {"x": 100, "y": 200, "text": "Click Me"}
  }]
}
```

**Normalized Internal Format:**
```lisp
'((:content . (((:type . "tool_use")
                (:name . "create_button")
                (:input . ((:x . 100) (:y . 200) (:text . "Click Me")))))))
```

The `parse-openai-response` function converts OpenAI/Groq responses to match the internal format:

```lisp
(defun parse-openai-response (response)
  "Parse OpenAI API response and normalize to standard format"
  (let* ((choices (cdr (assoc :choices response)))
         (message (when choices (cdr (assoc :message (first choices)))))
         (tool-calls (when message (cdr (assoc :tool-calls message)))))

    (if tool-calls
        ;; Convert OpenAI tool_calls to standard format
        `((:content . ,(mapcar (lambda (tc)
                                (let* ((func (cdr (assoc :function tc)))
                                       (name (cdr (assoc :name func)))
                                       (args-str (cdr (assoc :arguments func)))
                                       (args (parse-json args-str)))
                                  `((:type . "tool_use")
                                    (:name . ,name)
                                    (:input . ,args))))
                              tool-calls)))
        ;; No tool calls - return text content
        `((:content . ,(cdr (assoc :content message)))))))
```

## Files Modified

### backend/src/ai-agent.lisp

**Added:**
- `parse-openai-response()` - Normalizes OpenAI/Groq responses
- `call-llm-api()` - Provider-agnostic entry point
- `call-openai-compatible-api()` - OpenAI/Groq API client
- `convert-messages-openai()` - Convert messages to OpenAI format
- `convert-tools-openai()` - Convert tools to OpenAI function calling format
- `*provider-config*` - Provider configuration registry
- `get-provider-config()` - Retrieve provider configuration
- LangChain-style environment variable configuration

**Modified:**
- `execute-ai-command()` - Now calls `call-llm-api()` instead of `call-claude-api()`
- `extract-tool-calls()` - Updated docstring to reflect provider-agnostic usage

### backend/src/websocket-adapter.lisp

**Fixed:**
- Syntax error in `handle-ai-command-message()` (extra closing parenthesis)

### backend/collabcanvas.asd

**Already had:**
- `:dexador` dependency for HTTP client
- `:quri` dependency for URL handling

## Usage

### Environment Variables

Configure the AI provider using environment variables:

```bash
# Use Groq with Llama model (recommended for cost/speed)
export AI_PROVIDER=groq
export GROQ_API_KEY=your_groq_api_key_here
export AI_MODEL=llama-3.3-70b-versatile  # Optional, has smart defaults

# Or use OpenAI with GPT-4o
export AI_PROVIDER=openai
export OPENAI_API_KEY=your_openai_api_key_here
export AI_MODEL=gpt-4o-mini  # Optional

# Or use Anthropic Claude
export AI_PROVIDER=anthropic
export ANTHROPIC_API_KEY=your_anthropic_api_key_here
export AI_MODEL=claude-3-5-sonnet-20241022  # Optional
```

### Starting the Server

**With Groq (current configuration):**
```bash
cd backend
AI_PROVIDER=groq GROQ_API_KEY=your_key_here ./start.sh
```

**With OpenAI:**
```bash
cd backend
AI_PROVIDER=openai OPENAI_API_KEY=your_key_here ./start.sh
```

**With Anthropic:**
```bash
cd backend
AI_PROVIDER=anthropic ANTHROPIC_API_KEY=your_key_here ./start.sh
```

### Model Selection

If `AI_MODEL` is not specified, the system uses smart defaults:

| Provider   | Default Model              | Notes                    |
|------------|----------------------------|--------------------------|
| OpenAI     | gpt-4o-mini               | Cost-effective           |
| Groq       | llama-3.3-70b-versatile   | Fast inference           |
| Anthropic  | claude-3-5-sonnet-20241022| Best for complex tasks   |

### WebSocket Message Format

The AI command message format remains unchanged:

```json
{
  "type": "ai-command",
  "command": "create a login form at position 100, 100 with dark theme"
}
```

The backend handles provider selection transparently.

## Supported Models

### OpenAI
- **gpt-4o** - Latest GPT-4 optimized model
- **gpt-4o-mini** - Cost-effective variant (default)
- **gpt-4-turbo** - Fast GPT-4 variant

### Groq
- **llama-3.3-70b-versatile** - Latest Llama 3.3 (default, recommended)
- **llama-3.1-70b-versatile** - Llama 3.1
- **mixtral-8x7b-32768** - Mixtral MoE model

### Anthropic
- **claude-3-5-sonnet-20241022** - Latest Claude Sonnet (default)
- **claude-3-opus-20240229** - Most capable Claude model

## Performance Characteristics

| Provider   | Latency  | Cost      | Function Calling | Notes                    |
|------------|----------|-----------|------------------|--------------------------|
| Groq       | ~300ms   | Low       | Excellent        | Fast inference, good for real-time |
| OpenAI     | ~1-2s    | Medium    | Excellent        | Reliable, widely supported |
| Anthropic  | ~2-3s    | High      | Excellent        | Best reasoning, high quality |

## Error Handling

The system provides clear error messages when:

- API key is missing or invalid
- Provider is not configured
- API request fails
- Model is unavailable
- Rate limits are exceeded

Example error response:
```json
{
  "type": "ai-command-error",
  "error": "No API key found. Set OPENAI_API_KEY, GROQ_API_KEY, or ANTHROPIC_API_KEY",
  "command": "create a button"
}
```

## Testing

### Manual Testing

1. Start the backend with desired provider
2. Open the frontend at http://localhost:5173
3. Log in and create a canvas
4. Send an AI command via chat: "create a login form at 100, 100"
5. Verify objects are generated correctly

### Provider Switching Test

Test all three providers to ensure compatibility:

```bash
# Test Groq
AI_PROVIDER=groq GROQ_API_KEY=$GROQ_KEY ./start.sh

# Test OpenAI
AI_PROVIDER=openai OPENAI_API_KEY=$OPENAI_KEY ./start.sh

# Test Anthropic
AI_PROVIDER=anthropic ANTHROPIC_API_KEY=$ANTHROPIC_KEY ./start.sh
```

## Future Enhancements

### Planned Improvements

1. **Provider fallback**: Automatically try alternative providers on failure
2. **Load balancing**: Distribute requests across multiple providers
3. **Cost tracking**: Monitor API usage and costs per provider
4. **Model comparison**: A/B testing framework for different models
5. **Streaming responses**: Support streaming for real-time feedback
6. **Custom prompts**: Provider-specific system prompts for better results

### Additional Providers

Future provider support could include:

- **Cohere**: Command models
- **AI21**: Jurassic models
- **Mistral AI**: Mixtral models (direct API, not via Groq)
- **Local models**: Ollama, LocalAI, vLLM

## Migration Notes

### Breaking Changes

None - the API interface remains unchanged from the user's perspective.

### Configuration Migration

If you were using the old Claude-only implementation:

**Old:**
```lisp
;; Hard-coded Claude configuration
(defparameter *claude-api-key* ...)
```

**New:**
```bash
# Set environment variables instead
export AI_PROVIDER=anthropic
export ANTHROPIC_API_KEY=your_key_here
```

## Troubleshooting

### "No API key found" error

Ensure you've set the appropriate environment variable:
```bash
echo $GROQ_API_KEY      # Check Groq key
echo $OPENAI_API_KEY    # Check OpenAI key
echo $ANTHROPIC_API_KEY # Check Anthropic key
```

### "Unknown provider" error

Check `AI_PROVIDER` is set to one of: `openai`, `groq`, `anthropic`

```bash
echo $AI_PROVIDER  # Should be openai, groq, or anthropic
```

### API timeout errors

Increase timeout in `backend/src/ai-agent.lisp`:
```lisp
(defparameter *ai-timeout* 30  ; Increase to 60 for slower providers
  "Timeout for AI API calls in seconds")
```

### Tool call parsing errors

Check logs for JSON parsing issues:
```bash
# Backend will log:
[WS AI] Processing command from username: create a button
[WS AI ERROR] AI command failed: <error details>
```

## References

- [OpenAI API Documentation](https://platform.openai.com/docs/api-reference)
- [Groq API Documentation](https://console.groq.com/docs)
- [Anthropic API Documentation](https://docs.anthropic.com/claude/reference)
- [LangChain Multi-Provider Patterns](https://python.langchain.com/docs/integrations/llms/)

## Conclusion

The multi-provider refactor provides flexibility, cost optimization, and redundancy while maintaining a clean, unified interface. The LangChain-style configuration makes it easy to switch providers via environment variables without code changes.

**Current Production Configuration:**
- Provider: Groq
- Model: llama-3.3-70b-versatile
- Status: Running successfully on port 8080
