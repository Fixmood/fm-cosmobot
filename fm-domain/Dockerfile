FROM docker.1ms.run/library/python:3.11-slim-bookworm
WORKDIR /app
RUN pip install --no-cache-dir --index-url https://pypi.tuna.tsinghua.edu.cn/simple \
    --timeout 300 --retries 5 Pillow==11.3.0
COPY app.py /app/app.py
ENTRYPOINT ["python3", "/app/app.py"]
