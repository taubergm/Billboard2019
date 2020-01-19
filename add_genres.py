import csv
import sys




# shows acoustic features for tracks for the given artist

#from __future__ import print_function    # (at top of module)
from spotipy.oauth2 import SpotifyClientCredentials
import json
import spotipy
import time
import sys
import pprint
import re

#client_credentials_manager = SpotifyClientCredentials()
client_credentials_manager = SpotifyClientCredentials(client_id="0c2ec7f161354a3a9b9ada95b72a1982", client_secret="d986f32ea3f74f1e9040482abb86849d")
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)
sp.trace=True



def get_major_genre_from_artist(artist_urn):
    artist = sp.artist(artist_urn)
    #pprint.pprint(artist)
    #print(artist['genres'])
    print(artist['genres'][-1:])



csvFile = "songs_2019b.csv"
outCsv = open('songs_2019_allgenre.csv', 'wb')
fieldnames = ['date',	'title',	'artist',	'peak_pos',	'last_week',	'weeks',	'rank',	'change',	'spotifyLink',	'spotifyID',	'videoLink', 'all_genre', 'main_genre']
writer = csv.DictWriter(outCsv, fieldnames=fieldnames)
writer.writeheader()

trackId = ""
trackUri = ""
with open(csvFile) as billboard:
    reader = csv.DictReader(billboard)
    for row in reader:

        # remove featuring artists and do general cleanup
        row['artist'] = row['artist'].lower()
        row['artist'] = re.sub(" featuring.*", '', row['artist'] )
        row['artist'] = re.sub(" with .*", '', row['artist'] )
        row['artist'] = re.sub(" & .*", '', row['artist'] )
        row['artist'] = re.sub(" \/ .*", '', row['artist'] )
        row['artist'] = re.sub(" x .*", '', row['artist'] )
        row['artist'] = re.sub(" duet.*", '', row['artist'] )
        row['artist'] = re.sub("travi$", "travis", row['artist'] )
        row['artist'] = re.sub("jay z", "jay-z", row['artist'] )
        row['artist'] = re.sub("\\\"misdemeanor\\\"", "misdemeanor",  row['artist'] )
        row['artist'] = re.sub(" + .*", '', row['artist'] )
        row['artist'] = re.sub(" vs.*", '', row['artist'] )

        print row['artist'] 


        result = sp.search(q=row['artist'], type='artist')
        #pprint.pprint(result)
        try:
            artist_urn = result['artists']['items'][0]['uri']  
            genres = result['artists']['items'][0]['genres']  
            main_genre = result['artists']['items'][0]['genres'][-1:]
            row['all_genre'] = genres
            row['main_genre'] = main_genre
        except:
            row['main_genre'] = "unknown"
            
        print row
        writer.writerow(row)
               

# so the flow is

# get unique tracks from R list of 15 yr songs
# get missing spotify track ids via search
# from spotify add audio features to the csv   and put all this shit in a SQL database where we don't duplicate data
# from spotify add genre to each row based on artist
#  get complete data with track, artist, audio features, genre
# add lyrics?
# plot the genre over time (first as pie charts from 2000, 2017 - then as line charts over time x axis, percentage of total y axis
# plot song duration historam in seconds, and duration v time (year on x axis)

# post is titles changing musical tastes



