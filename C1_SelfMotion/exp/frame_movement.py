import time, os, random, sys, json, copy
import numpy as np
import pandas as pd

from psychopy import prefs
prefs.hardware['audioLib'] = ['PTB']
from psychopy import visual, core, event, monitors, tools, sound
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


def run_exp(expno=1, setup='tablet', ID=np.nan):

    cfg = {}
    cfg['expno'] = expno
    cfg['expstart'] = time.time()

    # get participant ID, set up data folder for them:
    cfg = getParticipant(cfg, ID=ID)

    cfg = setWindow(cfg, setup=setup)

    # set up monitor and visual objects:
    cfg = getStimuli(cfg)

    # set up blocks and trials/tasks within them:
    cfg = getTasks(cfg)
    cfg = getMaxAmplitude(cfg)

    # try-catch statement in which we try to run all the tasks:
    # each trial saves its own data?
    # at the end a combined data file is produced?
    try:
        # run the tasks
        cfg = runTasks(cfg)
    except Exception as e:
        # do this in case of error:
        print('there was an error:')
        print(e)
    else:
        # if there is no error: export data as csv
        cfg = exportData(cfg)
    finally:
        # always do this:

        # save cfg, except for hardware related stuff (window object and stimuli pointing to it)
        saveCfg(cfg)

        # shut down the window object
        cleanExit(cfg)


def exportData(cfg):

    responses = cfg['responses']

    # collect names of data:
    columnnames = []
    for response in responses:
        rks = list(response.keys())
        addthese = np.nonzero([not(rk in columnnames) for rk in rks])[0]
        # [x+1 if x >= 45 else x+5 for x in l]
        [columnnames.append(rks[idx]) for idx in range(len(addthese))]

    # make dict with columnnames as keys that are all empty lists:
    respdict = dict.fromkeys(columnnames)
    columnnames = list(respdict)
    for rk in respdict.keys():
        respdict[rk] = []

    #respdict = {}
    #for colname in columnnames:
    #    respdict[colname] = []

    # go through responses and collect all data into the dictionary:
    for response in responses:
        for colname in columnnames:
            if colname in list(response.keys()):
                respdict[colname] += [response[colname]]
            else:
                respdict[colname] += ['']

    #for rk in respdict.keys():
    #    print([rk, len(respdict[rk])])

    pd.DataFrame(respdict).to_csv('%sresponses.csv'%(cfg['datadir']), index=False)

    print('data exported')

    return(cfg)

def doTrial(cfg):

    trialtype = cfg['blocks'][cfg['currentblock']]['trialtypes'][cfg['currenttrial']]
    trialdict = cfg['conditions'][trialtype]

    if 'record_timing' in trialdict.keys():
        record_timing = trialdict['record_timing']
    else:
        record_timing = False

    # straight up copies from the PsychoJS version:
    period = trialdict['period']
    #frequency = 1/copy.deepcopy(trialdict['period'])
    distance = trialdict['amplitude']

    if 'framesize' in trialdict.keys():
        framesize = trialdict['framesize']
    else:
        framesize = [6,5]

    cfg['hw']['white_frame'].width = framesize[0]
    cfg['hw']['white_frame'].height = framesize[0]
    cfg['hw']['gray_frame'].width = framesize[1]
    cfg['hw']['gray_frame'].height = framesize[1]

    #print('frame size set...')

    # if 'frameoffset' in trialdict.keys():
    #     frameoffset = trialdict['frameoffset']
    # else:
    #     frameoffset = [0,0]

    if 'mapping' in trialdict.keys():
        mapping = trialdict['mapping']
    else:
        mapping = [-1,1][random.randint(0,1)]
        trialdict['mapping'] = mapping

    # change frequency and distance for static periods at the extremes:
    if (0.35 - period) > 0:
        # make sure there is a 350 ms inter-flash interval
        extra_frames = int( np.ceil( (0.35 - period) / (1/30) ) )
    else:
        extra_frames = 4

    extra_time = (extra_frames/30)

    p = period + extra_time
    d = (distance/period) * p


    # DO THE TRIAL HERE
    trial_start_time = time.time()


    previous_frame_time = 0
    # # # # # # # # # #
    # WHILE NO RESPONSE

    frame_times  = []
    mouse_pos_X  = []
    frame_pos_X  = []
    blue_on      = []
    red_on       = []
    toggle_tick  = []
    percepts     = []

    # we show a blank screen for 1/3 - 2.3 of a second (uniform dist):
    blank = 1/3 + (random.random() * 1/3)

    # the frame motion gets multiplied by -1 or 1:
    xfactor = [-1,1][random.randint(0,1)]

    # the mouse response has a random offset between -3 and 3 degrees
    percept = (random.random() - 0.5) * 6

    waiting_for_response = True

    flashdot_centre = [-8, -8]
    cfg['hw']['bluedot'].pos = [flashdot_centre[0], flashdot_centre[1]+1]
    cfg['hw']['reddot'].pos = [flashdot_centre[0], flashdot_centre[1]-1]

    frame_centre = [flashdot_centre[0], flashdot_centre[1]]

    flash_red = False
    flash_blue = False

    red_tick = 0
    blue_tick = 0

    flash_start_time = 0

    #print('preparation done...')

    while waiting_for_response:

        # blank screen of random length between 1/3 and 2.3 seconds
        while (time.time() - trial_start_time) < blank:
            event.clearEvents(eventType='mouse')
            event.clearEvents(eventType='keyboard')
            cfg['hw']['win'].flip()

        # on every frame:
        this_frame_time = time.time() - trial_start_time
        frame_time_elapsed = this_frame_time - previous_frame_time
        #print(round(1/frame_time_elapsed))

        # shorter variable for equations:
        t = this_frame_time


        # sawtooth, scaled from -0.5 to 0.5
        offsetX = abs( ( ((t/2) % p) - (p/2) ) * (2/p) ) - 0.5
        offsetX = offsetX * d

        #flash_red  = False
        #flash_blue = False
        flash_frame = False

        tick_toggle = 0
        # flash any dots?
        if ( ((t + (1/30)) % (2*p)) < (1.75/30)):
            if red_tick == 0:
                red_tick = 1
                cfg['hw']['metronome'].play()
                tick_toggle = 1
            #flash_red = True
        else:
            red_tick = 0
        if ( ((t + (1/30) + (p/1)) % (2*p)) < (1.75/30) ):
            if blue_tick == 0:
                blue_tick = 1
                cfg['hw']['metronome'].play()
                tick_toggle = 1
            #flash_blue = True
        else:
            blue_tick = 0


    #    # flash frame for apparent motion frame:
    #    if ( ((t + (1/30)) % (p/1)) < (2/30)):
    #        flash_frame = True

    #    # correct frame position:
    #    if (abs(offsetX) >= (distance/2)):
    #        offsetX = np.sign(offsetX) * (distance/2)
    #    else:
    #        flash_frame = False

        # flip offset according to invert percepts:
        offsetX = offsetX * xfactor

        # show frame for the classic frame:
        if trialdict['stimtype'] in ['classicframe']:

            # flash any dots?
            flash_red = False
            flash_blue = False
            if ( ((t + (1/30) ) % (2*p)) < (1.75/30)):
                flash_red = True
            if ( ((t + (1/30) + (p/1) ) % (2*p)) < (1.75/30) ):
                flash_blue = True

            frame_pos = [offsetX+frame_centre[0], frame_centre[1]]
            cfg['hw']['white_frame'].pos = frame_pos
            cfg['hw']['white_frame'].draw()
            cfg['hw']['gray_frame'].pos = frame_pos
            cfg['hw']['gray_frame'].draw()


            # for movement data storage:
            mousepos = ['', '']
            mframeX = frame_pos

    #    # show frame for timed frame:
    #    if trialdict['stimtype'] in ['timedframe']:
    #        frame_pos = [offsetX+frame_centre[0], frame_centre[1]]
    #        if frame_on:
    #            cfg['hw']['white_frame'].pos = frame_pos
    #            cfg['hw']['white_frame'].draw()
    #            cfg['hw']['gray_frame'].pos = frame_pos
    #            cfg['hw']['gray_frame'].draw()





        if trialdict['stimtype'] in ['moveframe']:

            # in DEGREES:
            mousepos = cfg['hw']['mouse'].getPos()
            #print(mousepos[0])
            if abs(mousepos[0]) > cfg['trackextent']:
                mframeX = np.sign(mousepos[0]) * (distance/2)
                if (np.sign(mousepos[0]) == xfactor):
                    if not(flash_red):
                        flash_red = True
                        flash_start_time = this_frame_time
                else:
                    if not(flash_blue):
                        flash_blue = True
                        flash_start_time = this_frame_time

            else:
                mframeX = mousepos[0]/cfg['trackextent'] * (distance/2)
                flash_red = False
                flash_blue = False


            # if (this_frame_time - flash_start_time) > (2/30):
            #     flash_red = False
            #     flash_blue = False
            #     #flash_start_time = 0


            frame_pos = [(mframeX*mapping)+frame_centre[0], frame_centre[1]]
            cfg['hw']['white_frame'].pos = frame_pos
            cfg['hw']['white_frame'].draw()
            cfg['hw']['gray_frame'].pos = frame_pos
            cfg['hw']['gray_frame'].draw()

        #percept = (mousepos[0] + mouse_offset) / 4


        if flash_red:
            cfg['hw']['reddot'].draw()
        #    if not prev_flash_red:
        #        cfg['hw']['metronome'].play()
        if flash_blue:
            cfg['hw']['bluedot'].draw()
        #    if not prev_flash_blue:
        #        cfg['hw']['metronome'].play()


        if cfg['hw']['keyboard'][key.NUM_LEFT]:
            percept = percept - 0.05
        if cfg['hw']['keyboard'][key.NUM_RIGHT]:
            percept = percept + 0.05
        if cfg['hw']['keyboard'][key.LEFT]:
            percept = percept - 0.05
        if cfg['hw']['keyboard'][key.RIGHT]:
            percept = percept + 0.05

        # blue is on top:
        cfg['hw']['bluedot_ref'].pos = [percept-flashdot_centre[0], 1-flashdot_centre[0]]
        cfg['hw']['reddot_ref'].pos = [-percept-flashdot_centre[1],-1-flashdot_centre[1]]
        cfg['hw']['bluedot_ref'].draw()
        cfg['hw']['reddot_ref'].draw()

        cfg['hw']['win'].flip()

#        prev_flash_red  = flash_red
#        prev_flash_blue = flash_blue

        previous_frame_time = this_frame_time

        frame_times  += [this_frame_time]
        mouse_pos_X  += [mousepos[0]]
        frame_pos_X  += [frame_pos[0]]
        blue_on      += [flash_blue]
        red_on       += [flash_red]
        toggle_tick  += [tick_toggle]
        percepts     += [percept]


        # key responses:
        keys = event.getKeys(keyList=['space','rctrl','escape'])
        if len(keys):
            if 'rctrl' in keys:
                waiting_for_response = False
                reaction_time = this_frame_time - blank
            if 'escape' in keys:
                cleanExit(cfg)
        # alternative key-in key:
        if cfg['hw']['keyboard'][key.UP]:
            waiting_for_response = False
            reaction_time = this_frame_time - blank

        if record_timing and ((this_frame_time - blank) >= 3.0):
            waiting_for_response = False


    pd.DataFrame({'time':frame_times,
                  'mouseX':mouse_pos_X,
                  'frameX':frame_pos_X,
                  'blue_flashed':blue_on,
                  'red_flashed':red_on,
                  'toggle_tick':toggle_tick,
                  'percept':percepts}).to_csv('%stiming/b%d_t%d.csv'%(cfg['datadir'],cfg['currentblock'],cfg['currenttrial']), index=False)

    response                = trialdict
    response['xfactor']     = xfactor
    response['RT']          = reaction_time
    response['percept']     = percept
    response['trial_start'] = trial_start_time
    response['blank']       = blank

    cfg['responses'] += [response]

    #cfg['hw']['white_frame'].height=15
    #cfg['hw']['gray_frame'].height=14

    cfg['hw']['win'].flip()

    return(cfg)

def showInstruction(cfg):

    cfg['hw']['text'].text = cfg['blocks'][cfg['currentblock']]['instruction']

    waiting_for_response = True

    while waiting_for_response:

        cfg['hw']['text'].draw()
        cfg['hw']['win'].flip()

        keys = event.getKeys(keyList=['enter', 'return', 'escape'])
        if len(keys):
            if 'enter' in keys:
                waiting_for_response = False
            if 'return' in keys:
                waiting_for_response = False
            if 'escape' in keys:
                cleanExit(cfg)

def getPixPos(cfg):

    mousepos = cfg['hw']['mouse'].getPos() # this is in DEGREES
    pixpos = [tools.monitorunittools.deg2pix(mousepos[0], cfg['hw']['mon'], correctFlat=False),
              tools.monitorunittools.deg2pix(mousepos[1], cfg['hw']['mon'], correctFlat=False)]

    return(pixpos)

def runTasks(cfg):

    cfg = getMaxAmplitude(cfg)

    cfg['responses'] = []

    if not('currentblock' in cfg):
        cfg['currentblock'] = 0
    if not('currenttrial' in cfg):
        cfg['currenttrial'] = 0

    while cfg['currentblock'] < len(cfg['blocks']):

        # do the trials:
        cfg['currenttrial'] = 0

        showInstruction(cfg)

        while cfg['currenttrial'] < len(cfg['blocks'][cfg['currentblock']]['trialtypes']):

            trialtype = cfg['blocks'][cfg['currentblock']]['trialtypes'][cfg['currenttrial']]
            trialdict = cfg['conditions'][trialtype]

            if trialdict['stimtype'] in ['classicframe', 'moveframe', 'followframe']:

                cfg = doTrial(cfg)
                saveCfg(cfg)

            cfg['currenttrial'] += 1

        cfg['currentblock'] += 1



    return(cfg)

def setWindow(cfg, setup='tablet'):

    gammaGrid = np.array([[0., 1., 1., np.nan, np.nan, np.nan],
                          [0., 1., 1., np.nan, np.nan, np.nan],
                          [0., 1., 1., np.nan, np.nan, np.nan],
                          [0., 1., 1., np.nan, np.nan, np.nan]], dtype=float)
    # for vertical tablet setup:

        # gammaGrid = np.array([[0., 136.42685, 1.7472667, np.nan, np.nan, np.nan],
        #                       [0.,  26.57937, 1.7472667, np.nan, np.nan, np.nan],
        #                       [0., 100.41914, 1.7472667, np.nan, np.nan, np.nan],
        #                       [0.,  9.118731, 1.7472667, np.nan, np.nan, np.nan]], dtype=float)

    if setup == 'tablet':
        gammaGrid = np.array([[  0., 107.28029,  2.8466334, np.nan, np.nan, np.nan],
                              [  0.,  22.207165, 2.8466334, np.nan, np.nan, np.nan],
                              [  0.,  76.29962,  2.8466334, np.nan, np.nan, np.nan],
                              [  0.,   8.474467, 2.8466334, np.nan, np.nan, np.nan]], dtype=float)

        waitBlanking = True
        resolution = [1680, 1050]
        size = [43.2, 27.1]
        distance = 60


        wacomCM = resolution[0] / 31.1

    if setup == 'laptop':
    # for my laptop:
        waitBlanking = True
        resolution   = [1920, 1080]
        size = [34.5, 19.5]
        distance = 40

        wacomCM = resolution[0] / 29.5


    mymonitor = monitors.Monitor(name='temp',
                                 distance=distance,
                                 width=size[0])
    mymonitor.setGammaGrid(gammaGrid)
    mymonitor.setSizePix(resolution)

    cfg['gammaGrid']    = list(gammaGrid.reshape([np.size(gammaGrid)]))
    cfg['waitBlanking'] = waitBlanking
    #cfg['resolution']   = resolution

    cfg['hw'] = {}

    # to be able to convert degrees back into pixels/cm
    cfg['hw']['mon'] = mymonitor

    #cfg['hw']['groove'] = [ tools.monitorunittools.pix2deg( (resolution[0]/2) - (5*wacomOneCM), cfg['hw']['mon'], correctFlat=False),
    #                        tools.monitorunittools.pix2deg( (resolution[0]/2) + (5*wacomOneCM), cfg['hw']['mon'], correctFlat=False) ]

    cfg['trackextent'] = tools.monitorunittools.pix2deg( (5*wacomCM), cfg['hw']['mon'], correctFlat=False)

    # first set up the window and monitor:
    cfg['hw']['win'] = visual.Window( fullscr=True,
                                      size=resolution,
                                      units='deg',
                                      waitBlanking=waitBlanking,
                                      color=[0,0,0],
                                      monitor=mymonitor)

    res = cfg['hw']['win'].size
    cfg['resolution'] = [int(x) for x in list(res)]
    cfg['relResolution'] = [x / res[1] for x in res]

    # print(cfg['resolution'])
    # print(cfg['relResolution'])

    # we also need a mouse object:
    cfg['hw']['mouse'] = event.Mouse(visible=False, newPos=None, win=cfg['hw']['win'])

    # and a keyboard object:
    # (pyglet keyboard system)
    cfg['hw']['keyboard'] = key.KeyStateHandler()
    cfg['hw']['win'].winHandle.push_handlers(cfg['hw']['keyboard'])


    return(cfg)

def getStimuli(cfg):

    cfg['stim_offsets'] = [4,2]

    #dot_offset = 6
    dot_offset = np.tan(np.pi/6)*6
    cfg['dot_offset'] = np.tan(np.pi/6)*6
    cfg['hw']['bluedot'] = visual.Circle(win=cfg['hw']['win'],
                                         units='deg',
                                         size=[1,1],
                                         edges=180,
                                         lineWidth=0,
                                         fillColor=[-1,-1,1],
                                         pos=[0-cfg['stim_offsets'][0],dot_offset-cfg['stim_offsets'][1]])
    cfg['hw']['reddot'] = visual.Circle(win=cfg['hw']['win'],
                                         units='deg',
                                         size=[1,1],
                                         edges=180,
                                         lineWidth=0,
                                         fillColor=[1,-1,-1],
                                         pos=[0-cfg['stim_offsets'][0],-dot_offset-cfg['stim_offsets'][1]])
    #np.tan(np.pi/6)*6


    cfg['hw']['white_frame'] = visual.Rect(win=cfg['hw']['win'],
                                           width=7,
                                           height=7,
                                           units='deg',
                                           lineColor=None,
                                           lineWidth=0,
                                           fillColor=[1,1,1])
    cfg['hw']['gray_frame'] =  visual.Rect(win=cfg['hw']['win'],
                                           width=6,
                                           height=6,
                                           units='deg',
                                           lineColor=None,
                                           lineWidth=0,
                                           fillColor=[0,0,0])

    cfg['hw']['white_bar'] = visual.Rect(win = cfg['hw']['win'],
                                         width = 1,
                                         height = 7,
                                         units = 'deg',
                                         lineColor = None,
                                         lineWidth = 0,
                                         fillColor=[1,1,1])


    cfg['hw']['bluedot_ref'] = visual.Circle(win=cfg['hw']['win'],
                                         units='deg',
                                         size=[1,1],
                                         edges=180,
                                         lineWidth=0,
                                         fillColor=[-1,-1,1],
                                         pos=[0,0.20])
    cfg['hw']['reddot_ref'] = visual.Circle(win=cfg['hw']['win'],
                                         units='deg',
                                         size=[1,1],
                                         edges=180,
                                         lineWidth=0,
                                         fillColor=[1,-1,-1],
                                         pos=[0,-0.20])

    # we also want to set up a mouse object:
    cfg['hw']['mouse'] = event.Mouse(visible=False, newPos=None, win=cfg['hw']['win'])

    # keyboard is not an object, already accessible through psychopy.event
    ## WAIT... it is an object now!
    #print('done this...')
    #cfg['hw']['keyboard'] = keyboard.Keyboard()
    #print('but not this?')

    # pyglet keyboard system:
    cfg['hw']['keyboard'] = key.KeyStateHandler()
    cfg['hw']['win'].winHandle.push_handlers(cfg['hw']['keyboard'])

    # but it crashes the system...

    cfg['hw']['text'] = visual.TextStim(win=cfg['hw']['win'],
                                        text='Hello!'
                                        )
    cfg['hw']['plus'] = visual.TextStim(win=cfg['hw']['win'],
                                        text='+',
                                        units='deg'
                                        )

    # The PYO way:
    # this sets up a short duration tick sound to use as a metronome
    tick = sound.load('short_tick.wav')
    cfg['hw']['metronome'] = sound.Sound(tick)

    # The PTB way:
    # this sets up a short duration tick sound to use as a metronome
    # tick = sound.load('short_tick.wav')
    cfg['hw']['metronome'] = sound.Sound('short_tick.wav')

    return(cfg)


def saveCfg(cfg):

    scfg = copy.copy(cfg)
    del scfg['hw']

    with open('%scfg.json'%(cfg['datadir']), 'w') as fp:
        json.dump(scfg, fp,  indent=4)


def getTasks(cfg):

    #if cfg['expno']==0:

        # period: 1.0, 1/2, 1/3, 1/4, 1/5
        # amplit: 2.4, 4.8, 7.2, 9.6, 12
        # (speeds: 12, 24, 36, 48, 60 deg/s)
    #    condictionary = [{'period':1.0, 'amplitude':12, 'stimtype':'classicframe'},
    #                     {'period':1/2, 'amplitude':12, 'stimtype':'classicframe'},
    #                     {'period':1/3, 'amplitude':12, 'stimtype':'classicframe'},
    #                     {'period':1/4, 'amplitude':12, 'stimtype':'classicframe'},
    #                     {'period':1/5, 'amplitude':12, 'stimtype':'classicframe'},
    #                     {'period':1/5, 'amplitude':2.4, 'stimtype':'classicframe'},
    #                     {'period':1/5, 'amplitude':4.8, 'stimtype':'classicframe'},
    #                     {'period':1/5, 'amplitude':7.2, 'stimtype':'classicframe'},
    #                     {'period':1/5, 'amplitude':9.6, 'stimtype':'classicframe'},
    #                     {'period':1/5, 'amplitude':12., 'stimtype':'classicframe'},
    #                     ]

    #    return( dictToBlockTrials(cfg=cfg, condictionary=condictionary, nblocks=1, nrepetitions=1) )

    if cfg['expno']==1:

        # period: 1.0, 1/2, 1/3, 1/4, 1/5
        # amplit: 2.4, 4.8, 7.2, 9.6, 12
        # (speeds: 12, 24, 36, 48, 60 deg/s)
        condictionary = [

                         {'period':1/2, 'amplitude':4, 'stimtype':'classicframe', 'framesize':[7,6], 'mapping': 1},
                         {'period':1/2, 'amplitude':4, 'stimtype':'moveframe',    'framesize':[7,6], 'mapping': 1},
                         {'period':1/2, 'amplitude':4, 'stimtype':'moveframe',    'framesize':[7,6], 'mapping':-1},
#                         {'period':1/3, 'amplitude':3, 'stimtype':'followframe',  'framesize':[6,5], 'mapping': 1},
#                         {'period':1/3, 'amplitude':3, 'stimtype':'followframe',  'framesize':[6,5], 'mapping':-1},
                         ]

        return( dictToBlockTrials(cfg=cfg, condictionary=condictionary, nblocks=3, nrepetitions=5) )


    if cfg['expno']==2:

        # period: 1.0, 1/2, 1/3, 1/4, 1/5
        # amplit: 2.4, 4.8, 7.2, 9.6, 12
        # (speeds: 12, 24, 36, 48, 60 deg/s)
        condictionary = [

                         {'period':1/2, 'amplitude':4, 'stimtype':'classicframe', 'framesize':[7,6], 'mapping': 1},
                         {'period':1/2, 'amplitude':4, 'stimtype':'moveframe',    'framesize':[7,6], 'mapping': 1},
                         {'period':1/2, 'amplitude':4, 'stimtype':'moveframe',    'framesize':[7,6], 'mapping':-1},
                         ]

        return( dictToBlockTrials(cfg=cfg, condictionary=condictionary, nblocks=1, nrepetitions=1) )


def dictToBlockTrials(cfg, condictionary, nblocks, nrepetitions):

    cfg['conditions'] = condictionary

    blocks = []
    for block in range(nblocks):

        blockconditions = []

        for repeat in range(nrepetitions):
            trialtypes = list(range(len(condictionary)))
            random.shuffle(trialtypes)
            blockconditions += trialtypes

        blocks += [{'trialtypes':blockconditions,
                    'instruction':'get ready for block %d of %d\npress enter to start'%(block+1,nblocks)}]

    cfg['blocks'] = blocks

    return(cfg)


def getMaxAmplitude(cfg):

    maxamplitude = 0
    for cond in cfg['conditions']:
        maxamplitude = max(maxamplitude, cond['amplitude'])

    cfg['maxamplitude'] = maxamplitude

    return(cfg)

def foldout(a):
  # http://code.activestate.com/recipes/496807-list-of-all-combination-from-multiple-lists/

  r=[[]]
  for x in a:
    r = [ i + [y] for y in x for i in r ]

  return(r)


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
        for thisPath in ['../data', '../data/exp_%d'%(cfg['expno']), '../data/exp_%d/p%03d'%(cfg['expno'],cfg['ID']), '../data/exp_%d/p%03d/timing'%(cfg['expno'],cfg['ID'])]:
            if os.path.exists(thisPath):
                if not(os.path.isdir(thisPath)):
                    os.makedirs
                    sys.exit('"%s" should be a folder'%(thisPath))
                else:
                    # if participant folder exists, don't overwrite existing data?
                    if (thisPath == '../data/exp_%d/p%03d'%(cfg['expno'],cfg['ID'])):
                        sys.exit('participant already exists (crash recovery not implemented)')
            else:
                os.mkdir(thisPath)

        cfg['datadir'] = '../data/exp_%d/p%03d/'%(cfg['expno'],cfg['ID'])

    # we need to seed the random number generator:
    random.seed(99999 * IDno)

    return cfg


def cleanExit(cfg):

    cfg['expfinish'] = time.time()

    saveCfg(cfg)

    print('cfg stored as json')

    cfg['hw']['win'].close()

    return(cfg)








run_exp(expno=int(sys.argv[1]), setup='tablet', ID=int(sys.argv[2]))
