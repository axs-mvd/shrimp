# main.py

from fastapi import FastAPI, Response, Request
from starlette.background import BackgroundTask
from typing import Dict, Any
import logging


app = FastAPI()
logging.basicConfig(level=logging.ERROR)


def log_info(req_body, res_body):
    logging.info(req_body)
    logging.info(res_body)


@app.middleware('http')
async def some_middleware(request: Request, call_next):
    logging.info('headers: ' + str(request.headers))
    req_body = await request.body()
    response = await call_next(request)
    
    chunks = []
    async for chunk in response.body_iterator:
        chunks.append(chunk)
    res_body = b''.join(chunks)
    
    task = BackgroundTask(log_info, req_body, res_body)
    return Response(content=res_body, status_code=response.status_code, 
        headers=dict(response.headers), media_type=response.media_type, background=task)


@app.post('/')
def main(payload: Dict[Any, Any]):
    return payload


@app.get("/api/kraken")
def read_root():
    return {"Hello": "World"}
