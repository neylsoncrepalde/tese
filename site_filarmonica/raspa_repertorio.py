#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan  4 17:07:16 2018
Raspa concertos da Orquestra Filarmônica de MG
@author: neylson
"""
from urllib.request import urlopen
from urllib.error import HTTPError
from bs4 import BeautifulSoup
import os
import csv
import time

ini = time.time()

def pegaPagina(url):
    # Get and parse url
    try:    
        html = urlopen(url)
    except HTTPError as e:
        return None
    bsObj = BeautifulSoup(html.read(), 'lxml')
    return bsObj

def raspaRepertorio(bsObj):
    # Get the timeline content
    timeline = bsObj.find_all(name="div", attrs={"class":"timeline-content"})
    
    # Get raw date and venue from inside the timeline list
    raw = []
    for tag in timeline [1:]:
        raw.append(tag.find(name="header").get_text())
    
    # Clean partially date and venue
    dia = []
    mes = []
    ano = []
    local = []
    for i in raw:
        x = i.replace('\n', '')
        y = x.replace('\t', '')
        z = y.replace('1º', '1')
        separado = z.split(', ')                    # separa data e local 02
        if len(separado) <= 2:
            data_separado = separado[0].split(' de ')   # separa dia, mês e ano 03
            
            dia.append(data_separado[0])
            mes.append(data_separado[1])
            ano.append(bsObj.find(name="span", attrs={"class":"h2"}).get_text())
            local.append(separado[1])
        elif len(separado) > 2:
            booleanos = [' de ' in i for i in separado]
            correto = [booleanos.index(i) for i in booleanos if i == True]
            data_separado = separado[correto[0]].split(' de ')   # separa dia, mês e ano 03
            
            dia.append(data_separado[0])
            mes.append(data_separado[1])
            ano.append(bsObj.find(name="span", attrs={"class":"h2"}).get_text())
            local.append("CONFERIR")
        
    # Pega o nome da série
    lista_series = bsObj.find_all(name="span", attrs={"class":"h3"})
    series = []
    for i in lista_series:
        series.append(i.get_text())
        
    # Pega os convidados
    convidados = {}
    for i in range(1, len(timeline)):
        convidados [i-1] = [j.get_text() for j in \
                   timeline[i].find_all(name="strong")]
        
    # Pega o repertório
    repertorio = {}
    for i in range(1, len(timeline)):
        lista_repertorio = timeline[i].find_all(name="em")
        pais = [j.parent.get_text() for j in lista_repertorio]
        unicos = list(set(pais))
        #repertorio_separado = repertorio_completo.split('\n')
        repertorio[i-1] = [z.split('\n') for z in unicos]
        
        
    # Escreve num csv
    saida = open('concertos_filarmonica_' + ano[0] + '.csv','w')
    export = csv.writer(saida, quoting=csv.QUOTE_NONNUMERIC)
    export.writerow(['ano','mes','dia','local','serie','artista','repertorio'])
    
    # Se der tdo certinho
    if len(dia) == len(convidados) == len(repertorio):
        # Loopa por repertorio e por convidado
        print("Gravando dados no csv...........")
        for i in range(len(dia)):
            for j in range(len(convidados[i])):
                for z in range(len(repertorio[i])):
                    for x in range(len(repertorio[i][z])):
                        export.writerow([ano[i], mes[i], dia[i], 
                                         local[i], series[i],
                                         convidados[i][j],
                                         repertorio[i][z][x]])
    
        saida.close()
        print("Dados gravados.")
    else:
        raise ValueError("Há divergência no tamanho dos vetores")
        
# ----------------------------------
# Coleta de Dados

os.chdir('/home/neylson/tese/site_filarmonica')

print("Coleta de dados.")
url_list = [
"http://www.filarmonica.art.br/filarmonica/temporadas-anteriores/?yearnum=2016#filtro",
"http://www.filarmonica.art.br/filarmonica/temporadas-anteriores/?yearnum=2015#filtro",
"http://www.filarmonica.art.br/filarmonica/temporadas-anteriores/?yearnum=2014#filtro",
"http://www.filarmonica.art.br/filarmonica/temporadas-anteriores/?yearnum=2013#filtro",
"http://www.filarmonica.art.br/filarmonica/temporadas-anteriores/?yearnum=2012#filtro",
"http://www.filarmonica.art.br/filarmonica/temporadas-anteriores/?yearnum=2011#filtro",
"http://www.filarmonica.art.br/filarmonica/temporadas-anteriores/?yearnum=2010#filtro",
"http://www.filarmonica.art.br/filarmonica/temporadas-anteriores/?yearnum=2009#filtro",
"http://www.filarmonica.art.br/filarmonica/temporadas-anteriores/?yearnum=2008#filtro",
]

for url in url_list:
    print("Coletando a url:\n" + url)
    soup = pegaPagina(url)

    print("Raspando a página.........")
    raspaRepertorio(soup)
    print("\n")

print("Raspagem completa.")    

fim = time.time()
print("A raspagem demorou {} segundos para rodar.".format(fim-ini))

