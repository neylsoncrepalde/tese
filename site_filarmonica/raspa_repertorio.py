#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan  4 17:07:16 2018

@author: neylson
"""
from urllib.request import urlopen
from urllib.error import HTTPError
from bs4 import BeautifulSoup
import os
import csv

def pegaPagina(url):
    try:    
        html = urlopen(url)
    except HTTPError as e:
        return None
    bsObj = BeautifulSoup(html.read(), 'lxml')
    return bsObj

def raspaRepertorio(bsObj):
    timeline = bsObj.find_all(name="div", attrs={"class":"timeline-content"})
    
    data_local = [x.find_all(name="header").get_text() for x in timeline]
    
    for tag in pagina.find_all(name="span", attrs={"class":"h3"}):
        print(tag.get_text())

