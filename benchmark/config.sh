#!/bin/bash

SHRIMP_ADMIN_HOST=localhost
SHRIMP_ADMIN_PORT=8000
SHRIMP_HOST=localhost
SHRIMP_PORT=8080

SHRIMP_ADMIN_URL="http://${SHRIMP_ADMIN_HOST}:${SHRIMP_ADMIN_PORT}/api"

SHRIMP_TARGET_URL="http://${SHRIMP_HOST}:${SHRIMP_PORT}"

BACKEND_HOST=localhost
BACKEND_PORT=9090
BACKEND_URL="http://${BACKEND_HOST}:${BACKEND_PORT}/kraken"

echo "creating backend"
curl -s -X POST ${SHRIMP_ADMIN_URL}/backend \
-H "Content-Type: application/json"      \
-d '{
    "name": "test-backend",
    "url": "'${BACKEND_URL}'",
    "pool-size": {"min": 1, "max": 5}
}' | jq .
echo ""


echo "creating rule"
curl -s -X POST ${SHRIMP_ADMIN_URL}/rule \
-H "Content-Type: application/json"   \
  -d '{
    "name": "test-rule",
    "in": "/api",
    "out": {
      "backends": ["test-backend"],
      "dispatcher": "random"
    },
    "middlewares": []
  }' | jq .
echo ""

