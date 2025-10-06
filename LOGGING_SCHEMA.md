# Jules Activity Log JSON Schema

This document defines the JSON schema for entries in `logs/activity.log.jsonl`. Each entry is a JSON object on a new line, representing a single action or event.

## Schema Definition

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "Jules Activity Log Entry",
  "description": "A single entry in the agent's activity log.",
  "type": "object",
  "properties": {
    "timestamp": {
      "description": "The ISO 8601 timestamp of the event.",
      "type": "string",
      "format": "date-time"
    },
    "event_type": {
      "description": "The type of event being logged.",
      "type": "string",
      "enum": ["tool_call", "tool_output", "observation", "error", "plan_update", "user_feedback"]
    },
    "tool_name": {
      "description": "The name of the tool being called. Null if not a tool_call event.",
      "type": ["string", "null"]
    },
    "tool_args": {
      "description": "The arguments passed to the tool. Null if not a tool_call event.",
      "type": ["object", "string", "null"]
    },
    "output": {
      "description": "The output from a tool call or an observation.",
      "type": ["string", "object", "null"]
    },
    "plan_step": {
      "description": "The current step of the plan being executed.",
      "type": "string"
    },
    "message": {
      "description": "A human-readable message describing the event or observation.",
      "type": "string"
    },
    "session_id": {
      "description": "A unique identifier for the current development session.",
      "type": "string"
    }
  },
  "required": ["timestamp", "event_type", "plan_step", "message", "session_id"]
}
```

## Example Entry

```jsonl
{"timestamp": "2025-10-26T10:00:00Z", "event_type": "tool_call", "tool_name": "list_files", "tool_args": {"path": "."}, "output": null, "plan_step": "1. Understand the codebase.", "message": "Listing files to understand the repository structure.", "session_id": "sess_12345abc"}
{"timestamp": "2025-10-26T10:00:01Z", "event_type": "tool_output", "tool_name": "list_files", "tool_args": null, "output": "['README.md', 'src/']", "plan_step": "1. Understand the codebase.", "message": "Received file listing.", "session_id": "sess_12345abc"}
```