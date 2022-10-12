import time, random, sys, os, json, copy
import numpy as np
import pandas as pd
from psychopy import visual, core, event, monitors, tools, sound
from psychopy.hardware import keyboard

# altenative keyboard read-out?
from pyglet.window import key

# this for our custom-made functions
#from common import run_exp, getParticipant, setWindow, getMaxAmplitude, showInstruction, cleanExit, dictToBlockTrials, saveCfg, getPixPos, exportData, foldout

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



def runall(demo=False):

    expno=1
    if demo:
        expno=2

    all_cfg = {}
    all_cfg = getParticipant(all_cfg, check_path=False)

    experiments = [
                    'A1', # space:         anaglyphs
                    'A2', # space:         probe distance
                    # NOT DOING THIS ONE:
                    #'A3', # space:         frame edge
                    'B1', # time:          apparent motion log
                    'B2', # time:          post passes
                    'C1', # motion-types:  self-motion
                    'C2', # motion-tyoes:  ldl-textures frame illusion
                    'C3'  # motion-types:  ldl-textures perceived frame motion
                  ]

    # different order for every participant:
    # because we set the seed with participant ID * 99999
    random.shuffle(experiments)

    for expcode in experiments:

        if expcode == 'A1':
            print('running A1: Anaglyph')

            os.chdir('A1_Anaglyph/exp/')

            # run it!
            os.system('ipython3 frame_depth.py %d %d'%(expno, all_cfg['ID']))

            os.chdir('../..')
            print('[exp A1 done]')
        if expcode == 'A2':
            print('running A2: Probe Distance')
            os.chdir('A2_ProbeDistance/exp/')

            # run it!
            os.system('ipython3 frame_space.py %d %d'%(expno, all_cfg['ID']))

            os.chdir('../..')
            print('[exp A2 done]')
        #if expcode == 'A3':
        #    os.chdir('A3_FullFrame/exp/')
        #    import something as curr_exp
             # run it!
             #curr_exp.run_exp(setup='laptop', expno=1, ID=cfg['ID'])
        #    os.chdir('../..')
        #    print('[exp A3 done]')

        if expcode == 'B1':
            print('running B1: Apparent Lag')
            os.chdir('B1_ApparentLag/exp/')

            # run it!
            os.system('ipython3 frame_apparent.py %d %d'%(expno, all_cfg['ID']))

            os.chdir('../..')
            print('[exp B1 done]')
        if expcode == 'B2':
            print('running B2: Pre/Post Diction')
            os.chdir('B2_PreDiction/exp/')

            # run it!
            os.system('ipython3 frame_time.py %d %d'%(expno, all_cfg['ID']))

            os.chdir('../..')
            print('[exp B2 done]')

        if expcode == 'C1':
            print('running C1: Self Motion')
            os.chdir('C1_SelfMotion/exp/')

            # run it!
            os.system('ipython3 frame_movement.py %d %d'%(expno, all_cfg['ID']))

            os.chdir('../..')
            print('[exp C1 done]')
        if expcode == 'C2':
            print('running C2: Dot-Texture Motion')
            os.chdir('C2_TextureMotion/exp/')

            # run it!
            os.system('ipython3 frame_background2.py %d %d'%(expno, all_cfg['ID']))

            os.chdir('../..')
            print('[exp C2 done]')
        if expcode == 'C3':
            print('running C3: Dot-Texture Perceived Frame Motion')
            os.chdir('C3_PerceivedMotion/exp/')

            # run it!
            os.system('ipython3 frame_motion.py %d %d'%(expno, all_cfg['ID']))

            os.chdir('../..')
            print('[exp C3 done]')


runall()
