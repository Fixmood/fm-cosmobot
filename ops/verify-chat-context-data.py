import sqlite3

connection = sqlite3.connect('file:/data/cosmobot.sqlite3?mode=ro', uri=True)
rows = connection.execute(
    """SELECT kind_key, COUNT(1), COUNT(DISTINCT chat_id)
       FROM chat_log
       WHERE recorded_at >= datetime('now', '-30 minutes')
       GROUP BY kind_key"""
).fetchall()
print(rows)
connection.close()
