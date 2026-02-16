  ![Shrimp Logo](shrimp-logo.png?raw=true "Shrimp - Lightweight HTTP Reverse Proxy")

# Shrimp 🦐

A lightweight, simple reverse HTTP proxy that's fully configurable through a REST API. Perfect for routing, load balancing, and managing your backend services without complex configuration files.

## Features

- **Simple REST API** - Configure everything on the fly without restarting
- **Lightweight** - Built in Erlang for speed and reliability
- **Backend Management** - Add, update, or remove backends dynamically
- **Routing Rules** - Define flexible routing rules with path-based forwarding
- **Load Balancing** - Built-in round-robin dispatcher
- **Middleware Support** - Extensible middleware pipeline
- **API Documentation** - Swagger/OpenAPI spec included

## Installation

### From Source

1. Clone the repository:
```bash
git clone https://github.com/yourusername/shrimp.git
cd shrimp
```

2. Build the project:
```bash
make
```

3. Start Shrimp:
```bash
_rel/shrimp_release/bin/shrimp_release start
```

The proxy will be available at `http://localhost:8000` and the API at `http://localhost:8000/api`.

## Quick Start

### 1. Create a Backend

```bash
curl -X POST http://localhost:8000/api/backend \
  -H "Content-Type: application/json" \
  -d '{
    "name": "service-1",
    "url": "http://backend1.example.com",
    "pool-size": 10
  }'
```

### 2. Create a Routing Rule

```bash
curl -X POST http://localhost:8000/api/rule \
  -H "Content-Type: application/json" \
  -d '{
    "name": "api-rule",
    "in": "/api",
    "out": {
      "backends": ["service-1"],
      "dispatcher": "round_robin"
    }
  }'
```

### 3. Forward a Request

```bash
curl http://localhost:8000/api/test
# Request is forwarded to http://backend1.example.com/api/test
```

## API Documentation

View the full API specification:

```bash
curl http://localhost:8000/doc
```

## Configuration

### Backends Endpoint
- `GET /api/backend` - List all backends
- `GET /api/backend/{name}` - Get a specific backend
- `POST /api/backend` - Create a new backend
- `PUT /api/backend/{name}` - Update a backend
- `DELETE /api/backend/{name}` - Delete a backend

### Rules Endpoint
- `GET /api/rule` - List all rules
- `GET /api/rule/{name}` - Get a specific rule
- `POST /api/rule` - Create a new rule
- `PUT /api/rule/{name}` - Update a rule
- `DELETE /api/rule/{name}` - Delete a rule

## Example: Multiple Backends with Load Balancing

```bash
# Create two backends
curl -X POST http://localhost:8000/api/backend \
  -H "Content-Type: application/json" \
  -d '{"name": "api-1", "url": "http://api1.example.com"}'

curl -X POST http://localhost:8000/api/backend \
  -H "Content-Type: application/json" \
  -d '{"name": "api-2", "url": "http://api2.example.com"}'

# Create a rule that load balances across both
curl -X POST http://localhost:8000/api/rule \
  -H "Content-Type: application/json" \
  -d '{
    "name": "balanced",
    "in": "/api",
    "out": {
      "backends": ["api-1", "api-2"],
      "dispatcher": "round_robin"
    }
  }'
```

## Building

```bash
make          # Build the project
make clean    # Clean build artifacts
make test     # Run tests
```

## Architecture

Shrimp is built with Erlang/OTP and uses:
- **Cowboy** - HTTP server and routing
- **Gun** - HTTP client for forwarding requests
- **JSX** - JSON encoding/decoding

## License

See LICENSE file for details.
