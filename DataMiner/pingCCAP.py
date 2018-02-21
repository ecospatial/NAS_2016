import urllib
import os

dir = os.path.dirname(os.path.realpath(__file__))

states = ['al','fl','ms','la','tx']
base_url = 'https://coast.noaa.gov/htdata/CCAP/ccap_regional_change/%s_2006_2010_CCAP_CHANGE.zip'

user_agent = 'Mozilla 5.0 (Windows 7; Win64; x64)'
headers = { 'User-Agent' : user_agent }

for state in states:
    url = base_url % state
    try:
        request = urllib2.Request(url, headers=headers)
        response = urllib2.urlopen(request)
        meta = response.info()
        file_size = int(meta.getheaders('Content-Length')[0])
        file_name = os.path.basename(url)
        local_file_path = "%s/%s" % (dir, file_name)
        if os.path.exists(local_file_path):
            print '%s already downloaded.' % file_name
            continue
        
        print ('Downloading: %s [%s bytes]\n%s' % (file_name, file_size, url))
        with open(local_file_path, "wb") as local_file:
            local_file.write(response.read())
            local_file.close()

        print 'Download (%s) complete!' % file_name
            
    except (urllib2.HTTPError, urllib2.URLError) as e:
        print(e.code)
        print(e.args)


