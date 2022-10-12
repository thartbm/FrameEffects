# do imports:

import numpy as np
#import scipy as sp
#import pandas as pd

#import pygame
#from pygame.locals import *

import psychopy
from psychopy import visual, core, data, event, logging, sound, gui, monitors

#import time

import random
#from random import seed



def runLuminanceTest(ncolors=18,channels='all'):

    # ncolors ideally results in integer RGB values
    # values for ncolors that work that way, are;
    #   2 (boring, the extremes)
    #   4
    #   6
    #   16
    #   18
    #

    # setup configuration object:
    cfg = {}

    # add a window for visual stimuli:
    cfg['fullscr']      = True
    cfg['monitorIndex'] = 1
    cfg['flip']         = True
    cfg['flip']         = False
    #resolution=[1680, 1050]
    cfg = createWindow(cfg, resolution=[1920, 1080])

    cfg = createStimuli(cfg)

    cfg = createTestColors(cfg, ncolors=ncolors, channels=channels)
    #cfg = createTestColors(cfg, ncolors=6, channels=['white'])

    # run the actual experiment, catch errors to cleanly exit:
    try:

        cfg = LuminanceTest(cfg)

    except Exception as err:

        # what went wrong?
        print(err)
        #print('some error?')

    finally:

        cfg = cleanlyExit(cfg)



def createWindow(cfg, resolution=[1920, 1080]):

    viewScale = [1, 1]
    if cfg['flip']:
        viewScale = [1, -1]

    gammaGrid = np.array([[  0., 107.28029,  2.8466334, np.nan, np.nan, np.nan],
                          [  0.,  22.207165, 2.8466334, np.nan, np.nan, np.nan],
                          [  0.,  76.29962,  2.8466334, np.nan, np.nan, np.nan],
                          [  0.,   8.474467, 2.8466334, np.nan, np.nan, np.nan]], dtype=float)

    mymonitor = monitors.Monitor(name='temp',
                                 distance=55,
                                 width=43.5)
    mymonitor.setGammaGrid(gammaGrid)
    mymonitor.setSizePix(resolution)

    win = visual.Window(resolution,
                        fullscr=cfg['fullscr'],
                        viewScale=viewScale,
                        waitBlanking=True,
                        monitor=mymonitor)


    cfg['win'] = win
    winsize = win.size
    cfg['width'] = winsize[0]
    cfg['height'] = winsize[1]
    cfg['winpos'] = cfg['win'].pos # might be necessary to subtract this from collected mouse coordinates?

    return(cfg)


def createStimuli(cfg):

    instruction_pos = [0,0.5]
    if (cfg['flip']):
        instruction_pos = [0,-0.5]
    # instruction text to show the RGB value used
    cfg['instruction'] = visual.TextStim(win=cfg['win'], text='', pos=instruction_pos, colorSpace='rgb', color=[1,1,1], flipVert=cfg['flip'])

    # rectangle?
    #w = cfg['width']
    #h = cfg['height']
    #if (w > h):
    #    rectsize = [w/h, 1]
    #else:
    #    rectsize = [1, h/w]

    cfg['patch'] = visual.Rect(win=cfg['win'], width=2, height=2, pos=[0,0])

    return(cfg)

def createTestColors(cfg, ncolors=18, channels='all'):

    # 4, 6 and 18 make for nice values (52 could work too)
    luminances = np.linspace(0,255,ncolors)

    colors=[]

    if channels == 'all':
        channels = ['white','R','G','B']

        for channel in channels:
            mask = {'white':[1,1,1],'R':[1,0,0],'G':[0,1,0],'B':[0,0,1]}[channel]

            for lum in luminances:
                # activities in each gun 0-255:
                color = [int(x * lum) for x in mask]
                # for display to experimenter:
                display = '[%d, %d, %d]'%(color[0],color[1],color[2])
                # psychopy color triplet -1:1
                ppcol = [(x * (2/255)) - 1 for x in color]
                # get hex color:
                hexcol = to_hex(color)

                colors.append({'rgb':color, 'text':display, 'psychopy':ppcol, 'hex':hexcol})

    if channels == 'random':

        random.seed(1337)

        R = list( np.linspace(0,255,18) )
        G = list( np.linspace(0,255,18) )
        B = list( np.linspace(0,255,18) )
        random.shuffle(R)
        random.shuffle(G)
        random.shuffle(B)

        W = np.linspace(0,255,4)
        random.shuffle(W)

        mask = [1,1,1]

        colors = []

        for lum in W:
            # activities in each gun 0-255:
            color = [int(x * lum) for x in mask]
            # for display to experimenter:
            display = '[%d, %d, %d]'%(color[0],color[1],color[2])
            # psychopy color triplet -1:1
            ppcol = [(x * (2/255)) - 1 for x in color]
            # get hex color:
            hexcol = to_hex(color)

            colors.append({'rgb':color, 'text':display, 'psychopy':ppcol, 'hex':hexcol})

        while len(R):

            for part in range(9):

                Ri = R.pop()
                Gi = G.pop()
                Bi = B.pop()

                color = [int(Ri), int(Gi), int(Bi)]
                display = '[%d, %d, %d]'%(color[0],color[1],color[2])
                ppcol = [(x * (2/255)) - 1 for x in color]
                hexcol = to_hex(color)

                colors.append({'rgb':color, 'text':display, 'psychopy':ppcol, 'hex':hexcol})

            random.shuffle(W)
            for lum in W:
                color = [int(x * lum) for x in mask]
                display = '[%d, %d, %d]'%(color[0],color[1],color[2])
                ppcol = [(x * (2/255)) - 1 for x in color]
                hexcol = to_hex(color)
                colors.append({'rgb':color, 'text':display, 'psychopy':ppcol, 'hex':hexcol})

    cfg['colors'] = colors

    return(cfg)

def to_hex(color):

    hexcol = '#'
    for colno in range(len(color)):
        hc = hex(color[colno])[2:4]
        if len(hc) == 1:
            hc = '0' + hc
        hexcol = hexcol + hc

    return(hexcol)


def cleanlyExit(cfg):

    cfg['win'].close()

    return(cfg)


def LuminanceTest(cfg):


    colors = cfg['colors']

    for color in colors:

        # set the 'patch' rectangle to the right color
        cfg['patch'].setFillColor(color=color['rgb'], colorSpace='rgb255')
        cfg['patch'].setLineColor(color=color['rgb'], colorSpace='rgb255')
        cfg['patch'].draw()

        # set instruction color to white or black depending average psychopy value of the patch
        cfg['instruction'].colorSpace = 'rgb255'
        if (np.mean(color['psychopy']) < 0):
            cfg['instruction'].color = [255,255,255]
        else:
            cfg['instruction'].color = [0,0,0]
        cfg['instruction'].text = color['text']
        cfg['instruction'].draw()

        # flip
        cfg['win'].flip()

        # wait for key press...
        event.waitKeys(keyList=['space'])

    return(cfg)
