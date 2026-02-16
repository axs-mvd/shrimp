# Shrimp API Documentation

The Shrimp API provides a REST interface for managing backends and routing rules. The API listens on port **8000** and is served through a separate Cowboy listener from the main reverse proxy.

## Base URL
```
http://localhost:8000/api
```

## Swagger Documentation
Interactive API documentation is available at:
- **UI**: `http://localhost:8000/api/doc`
- **YAML Spec**: `http://localhost:8000/api/doc/swagger.yaml`

## Endpoints

### Backends

#### List All Backends
```
GET /api/backend

Response: 200 OK
{
  "backendName": {
    "name": "backendName",
    "url": "http://backend-host:port",
    "pool_size": {
      "min": 1,
      "max": 10
    }
  }
}
```

#### Get Backend by Name
```
GET /api/backend/:name

Response: 200 OK
{
  "name": "backendName",
  "url": "http://backend-host:port",
  "pool_size": {
    "min": 1,
    "max": 10
  }
}

Response: 404 Not Found
```

#### Create Backend
```
POST /api/backend

Request:
{
  "name": "backendName",
  "url": "http://backend-host:port",
  "pool-size": {
    "min": 1,
    "max": 10
  }
}

Response: 201 Created (new)
Response: 200 OK (already exists)
Response: 400 Bad Request (invalid data)
```

#### Update Backend
```
PUT /api/backend/:name

Request:
{
  "name": "backendName",
  "url": "http://backend-host:port",
  "pool-size": {
    "min": 1,
    "max": 10
  }
}

Response: 200 OK
Response: 404 Not Found
Response: 400 Bad Request
```

#### Delete Backend
```
DELETE /api/backend/:name

Response: 200 OK
Response: 404 Not Found
```

### Rules

#### List All Rules
```
GET /api/rule

Response: 200 OK
[
  {
    "name": "ruleName",
    "in": "/path/pattern",
    "out": {
      "backends": ["backend1", "backend2"],
      "dispatcher": "round_robin"
    },
    "middlewares": []
  }
]
```

#### Get Rule by Name
```
GET /api/rule/:name

Response: 200 OK
{
  "name": "ruleName",
  "in": "/path/pattern",
  "out": {
    "backends": ["backend1"],
    "dispatcher": "round_robin"
  },
  "middlewares": []
}

Response: 404 Not Found
```

#### Create Rule
```
POST /api/rule

Request:
{
  "name": "ruleName",
  "in": "/path/pattern",
  "out": {
    "backends": ["backend1"],
    "dispatcher": "round_robin"
  },
  "middlewares": []
}

Response: 201 Created (new)
Response: 200 OK (already exists)
Response: 400 Bad Request
```

#### Update Rule
```
PUT /api/rule/:name

Request:
{
  "name": "ruleName",
  "in": "/path/pattern",
  "out": {
    "backends": ["backend1"],
    "dispatcher": "round_robin"
  },
  "middlewares": []
}

Response: 200 OK
Response: 404 Not Found
Response: 400 Bad Request
```

#### Delete Rule
```
DELETE /api/rule/:name

Response: 200 OK
Response: 404 Not Found
```

## HTTP Status Codes

- **200 OK**: Successful operation / Already exists
- **201 Created**: Resource created successfully
- **400 Bad Request**: Invalid or missing required fields
- **404 Not Found**: Resource does not exist
- **405 Method Not Allowed**: Invalid HTTP method for endpoint
- **5xx Server Error**: Internal server error

## Implementation Notes

- The API is served by a separate Cowboy HTTP listener to avoid conflicts with the main proxy
- All requests and responses use JSON format
- Pool size constraints: `min >= 0`, `max >= 0`, `min <= max`
- Backend names and URLs must be non-empty strings
- Rules must reference existing backends
- Dispatcher currently supports: `round_robin`
