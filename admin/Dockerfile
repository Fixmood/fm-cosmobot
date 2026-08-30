FROM python:3.11-slim-bookworm

RUN pip install --no-cache-dir websocket-client==1.8.0

WORKDIR /app
COPY app.py /app/app.py
COPY static /app/static

ENV FM_ADMIN_HOST=0.0.0.0
ENV FM_ADMIN_PORT=8090
ENV FM_ADMIN_STATE=/data/fm-admin-state.json
ENV FM_DOMAIN_URL=http://fm-domain:8077

VOLUME ["/data"]
EXPOSE 8090
ENTRYPOINT ["python3", "/app/app.py"]
