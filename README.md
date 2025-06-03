# Methodology for working with time series of climate data and microclimatic maps

NmetS - Methodologies approved by the relevant state administration body under whose competence the given issue falls

Project name: **Forest microclimate in time and space: real impacts of climate change on selected protected areas**\
Project number: **SS06010011**\
Project start: **04/2023**\
Project end: **03/2026**\
Project investigator: Institute of Botany of the Czech Academy of Sciences, Zámek 1, Průhonice, 25243, Czech Republic\
Confidentiality and availability: S – Complete and truthful project data not subject to protection under special legal regulations.\
Original release: **2024**
Version: **1.1**
Updated: 22.5.2025 (Update to myClim 1.40)
Tested with R 4.4.1, myClim 1.40, RStudio 2025.05.0

**Information about the author team:**\
RNDr. Josef Brůna, Ph.D., Principal Investigator\
Mgr. Tereza Klinerová, Research Team Member\
Mgr. Martin Kopecký, Ph.D., Research Team Member\
Mgr. Martin Macek, Ph.D., Research Team Member\
Mgr. Matěj Man, Research Team Member\
Mgr. Anna Růžičková, Research Team Member\
doc. Ing. Jan Wild, Ph.D., Research Team Member

#English version was translated by AI - Claude Sonnet 3.5 and manual checking was not done yet.

Original Czech version: <https://git.sorbus.ibot.cas.cz/vysledky/metodika-mikroklima-ppz>

![](images/logo_BU.png)

# Introduction

Microclimatic measurements are now widely available thanks to the development of automated autonomous measuring instruments, but their processing can be challenging due to large data volumes and non-standardized output data formats as well as non-standardized measurement methods depending on the specific research question. Therefore, they are only occasionally used in nature conservation (e.g., when assessing the impact of climate on protected features). The aim of this methodology is to facilitate the use of microclimatic data in nature conservation practice and ensure reproducibility of results through practical examples and proven procedures. The target user group is primarily staff of National Park and Protected Landscape Area administrations, as well as staff of the Nature Conservation Agency.

The first part of the methodology focuses on deriving biologically relevant variables, particularly based on time series from TOMST microclimatic stations, but also from other sources, so that other compatible data can be used as well. The main motivation is the fact that while classical meteorological data is already standardized and offers a number of comparable variables available from different sources, data from compact microclimatic stations is far from being as standardized and analysis of data obtained from individual stations can be challenging at first glance. Using examples based on real data, we will show the main problems in data processing and recommend proven procedures. The second part of the methodology focuses on the practical use of derived variables for data evaluation and also the use of microclimatic and climatic maps.

The methodology describes procedures for obtaining and processing microclimatic measurements from autonomous dataloggers (installation, data management, data quality control, processing and visualization of measured values, basic data analysis) completely in the open-source [R](https://www.r-project.org/) programming environment. We believe that the absence of ties to commercial software will facilitate its use. Another advantage of processing data in R is the reproducibility of procedures and results, as all data operations remain documented in the source code.

![](images/logo_tacr_spojene.png)
*This project is funded with state support from the Technology Agency of the Czech Republic and the Ministry of the Environment of the Czech Republic under the Environment for Life Program.*

The main text of the methodology is in the file: [**IBOT_Microclimate-methodology-2025-05-22.pdf**](https://git.sorbus.ibot.cas.cz/vysledky/microclimate-methodology/-/raw/main/IBOT_Microclimate-methodology-2025-05-22.pdf?ref_type=heads&inline=false).

Online version: <https://labgis.ibot.cas.cz/metodika-ppz-en/microclimate-methodology.html>

Chapters 1-4 are available as separate **.Rmd** files. For easier use, **.R** scripts of individual chapters are also published without text and images.