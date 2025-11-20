# Castle Game Engine MCP Server - Quick Reference

## Setup (30 seconds)

1. **Enable**: Edit → Preferences → MCP Server → ✓ Enable MCP Server
2. **Configure**: Set port (default: 3000)
3. **Start**: Click "Start Server"
4. **Verify**: Status shows "Running on port 3000"

## Essential API Calls

### Initialize Connection
```json
POST http://localhost:3000
{
  "jsonrpc": "2.0",
  "id": "1",
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-06-18",
    "capabilities": {},
    "clientInfo": {"name": "my-client", "version": "1.0.0"}
  }
}
```

### Get Project Info
```json
{
  "jsonrpc": "2.0",
  "id": "2",
  "method": "resources/read",
  "params": {"uri": "project://info"}
}
```

### Get Component Tree
```json
{
  "jsonrpc": "2.0",
  "id": "3",
  "method": "resources/read",
  "params": {"uri": "design://hierarchy"}
}
```

### Get Screenshot
```json
{
  "jsonrpc": "2.0",
  "id": "4",
  "method": "resources/read",
  "params": {"uri": "design://screenshot"}
}
```

### Read Property
```json
{
  "jsonrpc": "2.0",
  "id": "5",
  "method": "tools/call",
  "params": {
    "name": "get_component_property",
    "arguments": {
      "component_path": "MainForm.ButtonStart",
      "property_name": "Caption"
    }
  }
}
```

### Write Property
```json
{
  "jsonrpc": "2.0",
  "id": "6",
  "method": "tools/call",
  "params": {
    "name": "set_component_property",
    "arguments": {
      "component_path": "MainForm.ButtonStart",
      "property_name": "Caption",
      "value": "New Caption"
    }
  }
}
```

## Available Resources

| URI | Description | Type |
|-----|-------------|------|
| `project://info` | Project metadata, files | JSON |
| `design://hierarchy` | Component tree | JSON |
| `design://screenshot` | Design viewport image | PNG (base64) |

## Available Tools

| Tool | Description | Arguments |
|------|-------------|-----------|
| `get_project_info` | Project details | None |
| `get_component_property` | Read property | `component_path`, `property_name` |
| `set_component_property` | Write property | `component_path`, `property_name`, `value` |

## Common Component Paths

```
MainForm                    # Root view
MainForm.ButtonStart       # Button named "ButtonStart"
MainForm.Panel1.Label1     # Nested components
MainForm.Viewport.Scene    # 3D scene components
```

## Property Types

| Type | Example Value | JSON Format |
|------|---------------|-------------|
| String | "Hello World" | `"Hello World"` |
| Integer | 42 | `42` |
| Float | 3.14 | `3.14` |
| Boolean | true | `true` |
| Enum | alCenter | `"alCenter"` |

## Quick Troubleshooting

| Problem | Solution |
|---------|----------|
| Server won't start | Check port availability: `netstat -an \| grep :3000` |
| Can't connect | Verify status in Preferences, try different port |
| Property won't change | Check component path with hierarchy resource |
| Black screenshot | Ensure design is open and visible |

## One-Liner Tests

```bash
# Test connection
curl -X POST http://localhost:3000 -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","id":"1","method":"initialize","params":{}}'

# Get project info
curl -X POST http://localhost:3000 -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","id":"2","method":"resources/read","params":{"uri":"project://info"}}'

# Get component tree
curl -X POST http://localhost:3000 -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","id":"3","method":"resources/read","params":{"uri":"design://hierarchy"}}'
```

## Error Codes

| Code | Meaning | Action |
|------|---------|--------|
| -32700 | Parse error | Check JSON syntax |
| -32600 | Invalid request | Verify JSON-RPC format |
| -32601 | Method not found | Check method name |
| -32602 | Invalid params | Verify parameter format |
| -32603 | Internal error | Check server logs |

## Status Indicators

| Status | Meaning |
|--------|---------|
| "Stopped" | Server not running |
| "Running on port XXXX" | Server active and listening |
| Red text | Error state |
| Green text | Normal operation |

## Useful Commands

```bash
# Check if port is free
lsof -i :3000

# Test JSON-RPC with pretty output
curl -s -X POST http://localhost:3000 -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","id":"1","method":"initialize","params":{}}' | jq

# Monitor server logs
tail -f ~/.castle-engine/editor.log
```

## Integration Examples

### Python
```python
import requests
response = requests.post('http://localhost:3000', json={
    "jsonrpc": "2.0", "id": "1", "method": "initialize", "params": {}
})
```

### JavaScript
```javascript
const response = await fetch('http://localhost:3000', {
    method: 'POST',
    headers: {'Content-Type': 'application/json'},
    body: JSON.stringify({jsonrpc: "2.0", id: "1", method: "initialize", params: {}})
});
```

### cURL
```bash
curl -X POST http://localhost:3000 \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":"1","method":"initialize","params":{}}'
```

## Performance Tips

- Cache component hierarchy data
- Limit screenshot requests
- Use connection pooling
- Implement request timeouts
- Batch property changes when possible

## Security Notes

- Server only accepts localhost connections
- No authentication required (local only)
- All property changes are validated
- File access limited to project directory
- No arbitrary code execution
