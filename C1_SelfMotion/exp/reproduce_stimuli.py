import time, os, random, sys, json, copy
import numpy as np
import pandas as pd

from psychopy import prefs
prefs.hardware['audioLib'] = ['PTB']
from psychopy import visual, core, event, monitors, tools, sound
from psychopy.hardware import keyboard

# altenative keyboard read-out?
from pyglet.window import key



def repro_exp(expno=1, ID=np.nan):

    participant = []
    stimtype    = []
    period      = []
    amplitude   = []
    mapping     = []
    xfactor     = []

    for IDno in range(1,21):

        cfg = {}

        # this is code from getParticipant:

        # seed gets set to some unique number:
        random.seed(99999 * IDno)

        # this is code from getTasks:

        # now, a random random of stimuli is made:
        condictionary = [
                         {'period':1/2, 'amplitude':4, 'stimtype':'classicframe', 'framesize':[7,6], 'mapping': 1},
                         {'period':1/2, 'amplitude':4, 'stimtype':'moveframe',    'framesize':[7,6], 'mapping': 1},
                         {'period':1/2, 'amplitude':4, 'stimtype':'moveframe',    'framesize':[7,6], 'mapping':-1},
                         ]

        cfg = dictToBlockTrials(cfg=cfg, condictionary=condictionary, nblocks=3, nrepetitions=5)

        # this is code from runTasks:

        if not('currentblock' in cfg):
            cfg['currentblock'] = 0
        if not('currenttrial' in cfg):
            cfg['currenttrial'] = 0

        while cfg['currentblock'] < len(cfg['blocks']):

            # do the trials:
            cfg['currenttrial'] = 0

            while cfg['currenttrial'] < len(cfg['blocks'][cfg['currentblock']]['trialtypes']):

                trialtype = cfg['blocks'][cfg['currentblock']]['trialtypes'][cfg['currenttrial']]
                trialdict = cfg['conditions'][trialtype]

                if trialdict['stimtype'] in ['classicframe', 'moveframe', 'followframe']:

                    trialStimulus = doTrial(cfg)
                    print(trialStimulus)

                    participant += [IDno]
                    period      += [trialStimulus['period']]
                    amplitude   += [trialStimulus['amplitude']]
                    stimtype    += [trialdict['stimtype']]
                    mapping     += [trialStimulus['mapping']]
                    xfactor     += [trialStimulus['xfactor']]


                cfg['currenttrial'] += 1

            cfg['currentblock'] += 1

    pd.DataFrame({'participant':participant,
                  'period':period,
                  'amplitude':amplitude,
                  'stimtype':stimtype,
                  'mapping':mapping,
                  'xfactor':xfactor}).to_csv('../data/stimuli.csv', index=False)


def doTrial(cfg):
    trialtype = cfg['blocks'][cfg['currentblock']]['trialtypes'][cfg['currenttrial']]
    trialdict = cfg['conditions'][trialtype]

    period = trialdict['period']
    distance = trialdict['amplitude']

    if 'framesize' in trialdict.keys():
        framesize = trialdict['framesize']
    else:
        framesize = [6,5]

    if 'mapping' in trialdict.keys():
        mapping = trialdict['mapping']
    else:
        mapping = [-1,1][random.randint(0,1)]
        print("random mapping!")
        trialdict['mapping'] = mapping


    # we show a blank screen for 1/3 - 2.3 of a second (uniform dist):
    blank = 1/3 + (random.random() * 1/3)

    # the frame motion gets multiplied by -1 or 1:
    xfactor = [-1,1][random.randint(0,1)]

    # the mouse response has a random offset between -3 and 3 degrees
    percept = (random.random() - 0.5) * 6

    return( { 'period': period,
              'amplitude': distance,
              'mapping': mapping,
              'xfactor': xfactor } )




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
