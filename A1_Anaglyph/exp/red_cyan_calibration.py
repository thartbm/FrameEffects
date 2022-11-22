import time, os, random, sys, json, copy
import numpy as np
import pandas as pd
from psychopy import visual, core, event, monitors, tools
from psychopy.hardware import keyboard

# altenative keyboard read-out?
from pyglet.window import key

#To get psychtoolbox working for keyboard use you need the following steps to raise the priority of the experiment process. The idea is that to give you permission to do this without using super-user permissions to run your study (which would be bad for security) you need to add yourself to a group (e.g create a psychopy group) and then give that group permission to raise the priority of our process without being su:

#sudo groupadd --force psychopy
#sudo usermod -a -G psychopy $USER

#then do sudo nano /etc/security/limits.d/99-psychopylimits.conf and copy/paste in the following text to that file:

#@psychopy   -  nice       -20
#@psychopy   -  rtprio     50
#@psychopy   -  memlock    unlimited

def calibrate(setup='tablet', ID=np.nan):

    cfg = {}

    # set up calibration folder for participant:
    cfg = getParticipant(cfg, ID=ID)

    # set up psychopy window and stimuli objects:
    cfg = getStimuli(cfg, setup=setup)

    # run the calibration:
    cfg = showStimuli(cfg)

    # store the calibration:
    # scfg = copy.copy(cfg)
    # del scfg['hw']
    #
    # with open('%scfg.json'%(cfg['datadir']), 'w') as fp:
    #     json.dump(scfg, fp,  indent=4)
    calibration = {}
    calibration['RED']  = copy.copy(cfg['RED'])
    calibration['CYAN'] = copy.copy(cfg['CYAN'])
    with open('%sred_cyan_calibration.json'%(cfg['datadir']), 'w') as fp:
        json.dump(calibration, fp,  indent=4)



    cfg['hw']['win'].close()


def getStimuli(cfg, setup='tablet'):

    gammaGrid = np.array([[0., 1., 1., np.nan, np.nan, np.nan],
                          [0., 1., 1., np.nan, np.nan, np.nan],
                          [0., 1., 1., np.nan, np.nan, np.nan],
                          [0., 1., 1., np.nan, np.nan, np.nan]], dtype=float)
    # for vertical tablet setup:
    if setup == 'tablet':
        # gammaGrid = np.array([[0., 136.42685, 1.7472667, np.nan, np.nan, np.nan],
        #                       [0.,  26.57937, 1.7472667, np.nan, np.nan, np.nan],
        #                       [0., 100.41914, 1.7472667, np.nan, np.nan, np.nan],
        #                       [0.,  9.118731, 1.7472667, np.nan, np.nan, np.nan]], dtype=float)

        gammaGrid = np.array([[  0., 107.28029,  2.8466334, np.nan, np.nan, np.nan],
                              [  0.,  22.207165, 2.8466334, np.nan, np.nan, np.nan],
                              [  0.,  76.29962,  2.8466334, np.nan, np.nan, np.nan],
                              [  0.,   8.474467, 2.8466334, np.nan, np.nan, np.nan]], dtype=float)

        waitBlanking = True
        resolution = [1680, 1050]
        size = [47, 29.6]
        distance = 60

    if setup == 'laptop':
    # for my laptop:
        waitBlanking = True
        resolution   = [1920, 1080]
        size = [34.5, 19.5]
        distance = 40


    mymonitor = monitors.Monitor(name='temp',
                                 distance=distance,
                                 width=size[0])
    mymonitor.setGammaGrid(gammaGrid)
    mymonitor.setSizePix(resolution)

    # cfg['gammaGrid']    = list(gammaGrid.reshape([np.size(gammaGrid)]))
    # cfg['waitBlanking'] = waitBlanking
    # #cfg['resolution']   = resolution
    #
    cfg['hw'] = {}
    #
    # # to be able to convert degrees back into pixels/cm
    # cfg['hw']['mon'] = mymonitor

    # first set up the window and monitor:
    cfg['hw']['win'] = visual.Window( fullscr=True,
                                      size=resolution,
                                      units='deg',
                                      waitBlanking=waitBlanking,
                                      color=[0,0,0],
                                      monitor=mymonitor,
                                      useFBO=True,
                                      blendMode='add'
                                      )

    res = cfg['hw']['win'].size
    cfg['resolution'] = [int(x) for x in list(res)]
    cfg['relResolution'] = [x / res[1] for x in res]

    # IS THIS USED? NO!
    RED  = [0.8, 0.0859375, 0.0859375]
    CYAN = [0.0078125, 0.8, 0.8]

    cfg['hw']['top_cyan_dot'] = visual.Circle(win=cfg['hw']['win'],
                                         units='deg',
                                         size=[3,3],
                                         edges=180,
                                         lineWidth=0,
                                         fillColor=CYAN,
                                         pos=[0.5,3])
    cfg['hw']['top_red_dot'] = visual.Circle(win=cfg['hw']['win'],
                                         units='deg',
                                         size=[3,3],
                                         edges=180,
                                         lineWidth=0,
                                         fillColor=RED,
                                         pos=[-0.5,3])
    cfg['hw']['bottom_cyan_dot'] = visual.Circle(win=cfg['hw']['win'],
                                         units='deg',
                                         size=[3,3],
                                         edges=180,
                                         lineWidth=0,
                                         fillColor=CYAN,
                                         pos=[-0.5,-3])
    cfg['hw']['bottom_red_dot'] = visual.Circle(win=cfg['hw']['win'],
                                         units='deg',
                                         size=[3,3],
                                         edges=180,
                                         lineWidth=0,
                                         fillColor=RED,
                                         pos=[0.5,-3])
    #np.tan(np.pi/6)*6


    cfg['hw']['red_frame'] = visual.ShapeStim(win=cfg['hw']['win'],
                                                units='deg',
                                                colorSpace='rgb',
                                                lineColor=None,
                                                fillColor=RED,
                                                vertices=[[[-9,-9],[-9,9],[9,9],[9,-9],[-9,-9]],[[-8.5,-8.5],[-8.5,8.5],[8.5,8.5],[8.5,-8.5],[-8.5,-8.5]]]
                                                )
    cfg['hw']['cyan_frame'] = visual.ShapeStim(win=cfg['hw']['win'],
                                                units='deg',
                                                colorSpace='rgb',
                                                lineColor=None,
                                                fillColor=CYAN,
                                                vertices=[[[-9,-9],[-9,9],[9,9],[9,-9],[-9,-9]],[[-8.5,-8.5],[-8.5,8.5],[8.5,8.5],[8.5,-8.5],[-8.5,-8.5]]]
                                                )


    # we also want to set up a mouse object:
    cfg['hw']['mouse'] = event.Mouse(visible=False, newPos=None, win=cfg['hw']['win'])

    # pyglet keyboard system:
    cfg['hw']['keyboard'] = key.KeyStateHandler()
    cfg['hw']['win'].winHandle.push_handlers(cfg['hw']['keyboard'])

    # but it crashes the system...

    cfg['hw']['text'] = visual.TextStim(win=cfg['hw']['win'],
                                        text='Hello!'
                                        )

    cfg['hw']['red_text'] = visual.TextStim(win=cfg['hw']['win'],
                                        text='',
                                        pos=[-13,0]
                                        )
    cfg['hw']['green_text'] = visual.TextStim(win=cfg['hw']['win'],
                                        text='',
                                        pos=[12,0]
                                        )
    cfg['hw']['blue_text'] = visual.TextStim(win=cfg['hw']['win'],
                                        text='',
                                        pos=[16,0]
                                        )

    cfg['hw']['plus'] = visual.TextStim(win=cfg['hw']['win'],
                                        text='+',
                                        units='deg'
                                        )

    return(cfg)


def showStimuli(cfg):

    #    RED  = [0.8, 0.0859375, 0.0859375]
    #    CYAN = [0.0078125, 0.8, 0.8]

    RED_int =   127
    GREEN_int = 127
    BLUE_int =  127

    R = 0
    G = 0
    B = 0

    step = 0.001

    cfg['hw']['win'].flip()

    space_not_pressed = True

    while space_not_pressed:



        keys = event.getKeys(keyList=['space','escape'])
        if len(keys):
            if 'space' in keys:
                space_not_pressed = False


        #if cfg['hw']['keyboard'][key.NUM_1]:
        #    RED_int = max(RED_int-1, 0)
        #if cfg['hw']['keyboard'][key.NUM_2]:
        #    GREEN_int = max(GREEN_int-1, 0)
        #if cfg['hw']['keyboard'][key.NUM_3]:
        #    BLUE_int = max(BLUE_int-1, 0)
        #if cfg['hw']['keyboard'][key.NUM_7]:
        #    RED_int = min(RED_int+1, 255)
        #if cfg['hw']['keyboard'][key.NUM_8]:
        #    GREEN_int = min(GREEN_int+1, 255)
        #if cfg['hw']['keyboard'][key.NUM_9]:
        #    BLUE_int = min(BLUE_int+1, 255)

        #R = (RED_int   / 127.5) - 1
        #G = (GREEN_int / 127.5) - 1
        #B = (BLUE_int  / 127.5) - 1

        if cfg['hw']['keyboard'][key.NUM_1]:
            R = max(R-step, -1)
        if cfg['hw']['keyboard'][key.NUM_2]:
            G = max(G-step, -1)
        #if cfg['hw']['keyboard'][key.NUM_3]:
            B = max(B-step, -1)


        if cfg['hw']['keyboard'][key.NUM_7]:
            R = min(R+step,  1)
        if cfg['hw']['keyboard'][key.NUM_8]:
            G = min(G+step,  1)
        #if cfg['hw']['keyboard'][key.NUM_9]:
            B = min(B+step,  1)

        RED_int   = int(round((R + 1) * 127.5))
        GREEN_int = int(round((G + 1) * 127.5))
        BLUE_int  = int(round((B + 1) * 127.5))


        RED = [0.75, G, B]
        CYAN = [R, 0.75, 0.75]
        #RED = [R, 0, 0]
        #CYAN = [0, G, B]


        #RED  = [1,0,0]
        #CYAN = [0,1,1]

        #print(RED)
        #print(CYAN)


        cfg['hw']['red_text'].text = 'red:\n%d\n%0.3f'%(RED_int, R)
        cfg['hw']['green_text'].text = 'green:\n%d\n%0.3f'%(GREEN_int, G)
        cfg['hw']['blue_text'].text = 'blue:\n%d\n%0.3f'%(BLUE_int, B)
        cfg['hw']['red_text'].draw()
        cfg['hw']['green_text'].draw()
        cfg['hw']['blue_text'].draw()


        cfg['hw']['top_cyan_dot'].fillColor = CYAN
        cfg['hw']['top_red_dot'].fillColor = RED
        cfg['hw']['bottom_cyan_dot'].fillColor = CYAN
        cfg['hw']['bottom_red_dot'].fillColor = RED

        #print(cfg['hw']['bottom_red_dot'].fillColor)

        cfg['hw']['cyan_frame'].fillColor = CYAN
        cfg['hw']['red_frame'].fillColor = RED

        cfg['hw']['top_cyan_dot'].draw()
        cfg['hw']['top_red_dot'].draw()
        cfg['hw']['bottom_cyan_dot'].draw()
        cfg['hw']['bottom_red_dot'].draw()

        cfg['hw']['cyan_frame'].draw()
        cfg['hw']['red_frame'].draw()

        cfg['hw']['win'].flip()

    cfg['RED']  = RED
    cfg['CYAN'] = CYAN

    return(cfg)


def getParticipant(cfg, ID=np.nan, check_path=True):

    print(cfg)

    if np.isnan(ID):
        # we need to get an integer number as participant ID:
        IDnotANumber = True
    else:
        IDnotANumber = False
        cfg['ID'] = ID
        IDno = int(ID)

    # and we will only be happy when this is the case:
    while (IDnotANumber):
        # we ask for input:
        ID = input('Enter participant number: ')
        # and try to see if we can convert it to an integer
        try:
            IDno = int(ID)
            if isinstance(ID, int):
                pass # everything is already good
            # and if that integer really reflects the input
            if isinstance(ID, str):
                if not(ID == '%d'%(IDno)):
                    continue
            # only then are we satisfied:
            IDnotANumber = False
            # and store this in the cfg
            cfg['ID'] = IDno
        except Exception as err:
            print(err)
            # if it all doesn't work, we ask for input again...
            pass

    # set up folder's for groups and participants to store the data
    if check_path:
        for thisPath in ['../data', '../data/calibration', '../data/calibration/p%03d'%(cfg['ID'])]:
            if os.path.exists(thisPath):
                if not(os.path.isdir(thisPath)):
                    os.makedirs
                    sys.exit('"%s" should be a folder'%(thisPath))
                else:
                    # if participant folder exists, don't overwrite existing data?
                    if (thisPath == '../data/calibration/p%03d'%(cfg['ID'])):
                        sys.exit('participant already exists (crash recovery not implemented)')
            else:
                os.mkdir(thisPath)

        cfg['datadir'] = '../data/calibration/p%03d/'%(cfg['ID'])

    # we need to seed the random number generator:
    random.seed(99999 * IDno)

    return cfg







calibrate(setup='tablet', ID=int(sys.argv[1]))
