import xml.etree.ElementTree as ET
import csv

tree = ET.parse('export.xml') #parsing xml file
root = tree.getroot()

data = []

def removeNum(str): #this function just removes a number from the dates that is not supposed to be written to the csv file
    if '-0700' in str:
        return str.replace('-0700', '')
    return str

for record in root.findall('Record'): #finding Record element in xml file
    if record.get('type') == "HKCategoryTypeIdentifierSleepAnalysis": #if the type of record is sleep data, then extract creationDate, startDate, endDate
        creationDate = removeNum(record.get('creationDate'))
        startDate = removeNum(record.get('startDate'))
        endDate = removeNum(record.get('endDate'))
        if record.get('value') == "HKCategoryValueSleepAnalysisAsleepCore": #if the value is core, assign "core" to level and 1 to sleepWake
            level = "Core"
            sleepWake = '1'
        elif record.get('value') == "HKCategoryValueSleepAnalysisAwake": #awake and inbed is 0, everything else has a value of 0
            level = "Awake"
            sleepWake = '0'
        elif record.get('value') == "HKCategoryValueSleepAnalysisAsleepDeep":
            level = "Deep"
            sleepWake = '1'
        elif record.get('value') == "HKCategoryValueSleepAnalysisAsleepREM":
            level = "REM"
            sleepWake = '1'
        elif record.get('value') == "HKCategoryValueSleepAnalysisInBed":
            level = "InBed"
            sleepWake = '0'

        data.append([creationDate, startDate, endDate, level, sleepWake])

outputFile = 'output.csv' #name of the file writing the data to

with open(outputFile, mode='w', newline='') as file: #writing data
    writer = csv.writer(file)
    writer.writerow(['creationDate', 'startDate', 'endDate', 'level', 'sleepWake']) #headers in the columns
    writer.writerows(data)  #writing the rows