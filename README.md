(video showing the app: https://www.youtube.com/watch?v=B5eK1bEBE3c)

# Calliope
Experimental app written in Elm (0.17.1), run by Electron.

## Steps to run the app
If you want to try out this app, do the following:

### Clone the repository

Https:
``` git clone https://github.com/RobertWalter83/calliope.git ```

SSH:
``` git clone git@github.com:RobertWalter83/calliope.git ```

### Install Elm (if not alread on your machine)
Form [here](http://elm-lang.org/install) (app is written in Elm 0.17.1, might not work with newer versions)

### Compile App
* Open a terminal in the root folder of the app (by default this is "calliope")
* run ```elm-make App.elm --output=elm.js```

at the first time, this will ask you to add additional packages the app depends on. Approve the plan by typing 'y'.

### Install Electron-prebuilt
In the same terminal as before, run the command ```npm install electron-prebuilt``` to get Electron.

### Run the app
Fire up Electron (and thus the app) by running ```npm start```

