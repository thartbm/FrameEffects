import time, os, random, sys, json, copy
import numpy as np
import pandas as pd
from psychopy import visual, core, event, monitors, tools, sound
from psychopy.hardware import keyboard

# altenative keyboard read-out?
from pyglet.window import key


def run_exp(expno=1, setup='tablet', ID=np.nan):

    print(expno)

    cfg = {}
    cfg['expno'] = expno
    cfg['expstart'] = time.time()

    print(cfg)

    # get participant ID, set up data folder for them:
    # (function defined here)
    cfg = getParticipant(cfg, ID=ID)

    # define monitor Window for the current setup:
    # (function defined here)
    cfg = setWindow(cfg, setup=setup)

    # set up visual objects for the task:
    # (function defined per experiment)
    cfg = getStimuli(cfg)

    # set up blocks and trials/tasks within them:
    # (function defined per experiment)
    cfg = getTasks(cfg)
    # (function defined here)
    cfg = getMaxAmplitude(cfg)

    # try-catch statement in which we try to run all the tasks:
    # each trial saves its own data?
    # at the end a combined data file is produced?
    try:
        # run the tasks
        # (function defined here)
        cfg = runTasks(cfg)
    except Exception as e:
        # do this in case of error:
        print('there was an error:')
        print(e)
    else:
        # if there is no error: export data as csv
        # (function defined here)
        cfg = exportData(cfg)
    finally:
        # always do this:

        # save cfg, except for hardware related stuff (window object and stimuli pointing to it)
        # (function defined here)
        saveCfg(cfg)

        # shut down the window object
        # (function defined here)
        cleanExit(cfg)

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
        for thisPath in ['data', 'data/exp_%d'%(cfg['expno']), 'data/exp_%d/p%03d'%(cfg['expno'],cfg['ID'])]:
            if os.path.exists(thisPath):
                if not(os.path.isdir(thisPath)):
                    os.makedirs
                    sys.exit('"%s" should be a folder'%(thisPath))
                else:
                    # if participant folder exists, don't overwrite existing data?
                    if (thisPath == 'data/exp_%d/p%03d'%(cfg['expno'],cfg['ID'])):
                        sys.exit('participant already exists (crash recovery not implemented)')
            else:
                os.mkdir(thisPath)

        cfg['datadir'] = 'data/exp_%d/p%03d/'%(cfg['expno'],cfg['ID'])

    # we need to seed the random number generator:
    random.seed(99999 * IDno)

    return cfg

def setWindow(cfg, setup='tablet'):

    gammaGrid = np.array([[0., 1., 1., np.nan, np.nan, np.nan],
                          [0., 1., 1., np.nan, np.nan, np.nan],
                          [0., 1., 1., np.nan, np.nan, np.nan],
                          [0., 1., 1., np.nan, np.nan, np.nan]], dtype=float)
    # for vertical tablet setup:
    if setup == 'tablet':
        #gammaGrid = np.array([[0., 136.42685, 1.7472667, np.nan, np.nan, np.nan],
        #                      [0.,  26.57937, 1.7472667, np.nan, np.nan, np.nan],
        #                      [0., 100.41914, 1.7472667, np.nan, np.nan, np.nan],
        #                      [0.,  9.118731, 1.7472667, np.nan, np.nan, np.nan]], dtype=float)

        gammaGrid = np.array([[  0., 107.28029,  2.8466334, np.nan, np.nan, np.nan],
                              [  0.,  22.207165, 2.8466334, np.nan, np.nan, np.nan],
                              [  0.,  76.29962,  2.8466334, np.nan, np.nan, np.nan],
                              [  0.,   8.474467, 2.8466334, np.nan, np.nan, np.nan]], dtype=float)

        waitBlanking = True
        resolution = [1680, 1050]
        size = [47, 29.6]
        distance = 60

        wacomOneCM = resolution[0] / 31.1

    if setup == 'laptop':
    # for my laptop:
        waitBlanking = True
        resolution   = [1920, 1080]
        size = [34.5, 19.5]
        distance = 40

        wacomOneCM = resolution[0] / 29.5


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

    cfg['trackextent'] = tools.monitorunittools.pix2deg( (5*wacomOneCM), cfg['hw']['mon'], correctFlat=False)

    # first set up the window and monitor:
    cfg['hw']['win'] = visual.Window( fullscr=True,
                                      size=resolution,
                                      units='deg',
                                      waitBlanking=waitBlanking,
                                      color=[0,0,0],
                                      monitor=mymonitor)
                                      # for anaglyphs: blendmode='add' !!!

    res = cfg['hw']['win'].size
    cfg['resolution'] = [int(x) for x in list(res)]
    cfg['relResolution'] = [x / res[1] for x in res]

    # print(cfg['resolution'])
    # print(cfg['relResolution'])

    return(cfg)

def getMaxAmplitude(cfg):

    maxamplitude = 0
    for cond in cfg['conditions']:
        maxamplitude = max(maxamplitude, cond['amplitude'])

    cfg['maxamplitude'] = maxamplitude

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

def cleanExit(cfg):

    cfg['expfinish'] = time.time()

    saveCfg(cfg)

    print('cfg stored as json')

    cfg['hw']['win'].close()

    return(cfg)

def dictToBlockTrials(cfg, condictionary, nblocks, nrepetitions, shuffle=True):

    cfg['conditions'] = condictionary

    blocks = []
    for block in range(nblocks):

        blockconditions = []

        for repeat in range(nrepetitions):
            trialtypes = list(range(len(condictionary)))
            if shuffle:
                random.shuffle(trialtypes)
            blockconditions += trialtypes

        blocks += [{'trialtypes':blockconditions,
                    'instruction':'get ready for block %d of %d\npress enter to start'%(block+1,nblocks)}]

    cfg['blocks'] = blocks

    return(cfg)

def saveCfg(cfg):

    scfg = copy.copy(cfg)
    del scfg['hw']

    with open('%scfg.json'%(cfg['datadir']), 'w') as fp:
        json.dump(scfg, fp,  indent=4)

def getPixPos(cfg):

    mousepos = cfg['hw']['mouse'].getPos() # this is in DEGREES
    pixpos = [tools.monitorunittools.deg2pix(mousepos[0], cfg['hw']['mon'], correctFlat=False),
              tools.monitorunittools.deg2pix(mousepos[1], cfg['hw']['mon'], correctFlat=False)]

    return(pixpos)

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

    print('data exported to csv')

    return(cfg)

def foldout(a):
  # http://code.activestate.com/recipes/496807-list-of-all-combination-from-multiple-lists/

  r=[[]]
  for x in a:
    r = [ i + [y] for y in x for i in r ]

  return(r)
