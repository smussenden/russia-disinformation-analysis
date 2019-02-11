from readability import Document #from https://github.com/buriy/python-readability
import requests
import re
import feedparser

def cleanBody(body):
    body = re.sub(r'<.*?>','',body)
    body = re.sub(r'\n+', '\n', body)
    body = re.sub(r'  +', ' ', body)
    body = body.replace('’', '\'')
    body = body.replace('”', '\"')
    body = body.replace('“', '\"')
    return body

with open('rssfeeds.txt', 'r') as RSSs:
    for rss in RSSs:
        for entry in feedparser.parse(rss).entries:
            link = entry.link
            title = entry.title
            time = entry.published
            body = cleanBody(Document(requests.get(link).text).summary())
            print(title, time, link)
            print(body)
            print('\n\n\n')
            input()
