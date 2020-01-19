import requests
from bs4 import BeautifulSoup
import re
import csv
import sqlite3
import datetime

conn = sqlite3.connect('billboard_2019.sqlite')
conn.text_factory = bytes
cur = conn.cursor()

# Make some fresh tables using executescript()
cur.executescript('''

DROP TABLE IF EXISTS Headlines;


CREATE TABLE Headlines (
    id  INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
    date    TEXT, 
    url     TEXT UNIQUE,
    headline TEXT
);

''')


def quantize_date(date):
    """Quantizes the passed date to the nearest Saturday, since
    Billboard charts are always dated by Saturday.
    This behavior is consistent with the website, even though charts
    are released 11 days in advance.
    E.g., entering 2016-07-19 corresponds to the chart dated 2016-07-23.

    Args:
        date: The chart date as a string, in YYYY-MM-DD format.
    """
    year, month, day = map(int, date.split('-'))
    passedDate = datetime.date(year, month, day)
    passedWeekday = passedDate.weekday()
    if passedWeekday == 5:  # Saturday
        return date
    elif passedWeekday == 6:  # Sunday
        quantizedDate = passedDate + datetime.timedelta(days=6)
    else:
        quantizedDate = passedDate + datetime.timedelta(days=5 - passedWeekday)
    return str(quantizedDate)

stories = []   # array of stories from TMZ

OUTFILE = "billboard_2019b.csv"


num_days_to_get = 363

with open(OUTFILE, 'wb') as output_file:
    
    keys = ['date', 'artist', 'title', 'rank', 'peak_pos', 'last_week', 'weeks']
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    

    songlist = [dict() for x in range(101)]  # creat list of 100 track dicts
    
    
    # create an array og 


    for i in range(0,num_days_to_get,7):
        #day = datetime.datetime.now() - datetime.timedelta(days=21008) - datetime.timedelta(days=i) 
        day = datetime.datetime.now() - datetime.timedelta(days=i) 
        date = str(day.date())
        print date 
        date = quantize_date(date)
        url = "https://www.billboard.com/charts/hot-100/%s" % date

        print url
        page = requests.get(url)
        #print page.content
        
        soup = BeautifulSoup(page.content, 'html.parser')
        #print(soup.prettify())

        

        week_data = soup.find_all(class_='chart-element__information')

    
        i = 1
        for track in week_data:
            print i

            track = track.get_text()
            track_data = track.splitlines()


            songlist[i]['artist'] = track_data[2]
            songlist[i]['title'] = track_data[1]
            songlist[i]['rank'] = i
            songlist[i]['weeks'] = track_data[7]
            songlist[i]['last_week'] = track_data[5]
            songlist[i]['peak_pos'] = track_data[6]
            i = i + 1
            


        for song in songlist:

            print song
            song['date'] = date
            dict_writer.writerow(song)     

        
        



