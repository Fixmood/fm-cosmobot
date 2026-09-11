import importlib.util
import json
import tomllib

with open('/runtime/config.toml', 'rb') as source:
    config = tomllib.load(source)
token = config['rpc']['token']
request = json.dumps({'jsonrpc': '2.0', 'id': 'prefix-deploy-check', 'method': 'chat.list_sessions', 'params': {}})
if importlib.util.find_spec('websocket'):
    import websocket
    connection = websocket.create_connection('ws://fm-cosmobot:38765/rpc', header={'Authorization': 'Bearer ' + token}, timeout=10, http_no_proxy=['fm-cosmobot'])
    connection.send(request)
    reply = json.loads(connection.recv())
    connection.close()
elif importlib.util.find_spec('websockets'):
    import asyncio
    import websockets
    async def check():
        async with websockets.connect('ws://127.0.0.1:38765/rpc', additional_headers={'Authorization': 'Bearer ' + token}, open_timeout=10) as connection:
            await connection.send(request)
            return json.loads(await asyncio.wait_for(connection.recv(), 10))
    reply = asyncio.run(check())
else:
    raise RuntimeError('No WebSocket client library installed')
assert reply.get('id') == 'prefix-deploy-check', 'Unexpected RPC response ID'
assert 'result' in reply and 'error' not in reply, 'RPC returned an error'
print('Authenticated chat.list_sessions RPC: PASS (response content withheld)')
