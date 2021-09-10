def XML_open_file(fileXML,fileCSV):
    import xml.etree.ElementTree as ET

    global file_count
    file_count += 1

    try:
            tree = ET.parse(fileXML)
    except:
        print('Error with tree at file number', file_count, fileXML)
        return

    root = tree.getroot()

    Paper_data = open(fileCSV, 'a+', newline='')
    csvwriter = csv.writer(Paper_data)

    for member in root.findall('content'):
        for paper in member.findall('article_rec'):
            pdate = ' '
            paffiliation = ''
            pdate = paper.find('article_publication_date').text
            for authors in paper.findall('authors'):
                for au in authors.findall('au'):
                    pfirstname =''
                    pmiddlename =''
                    plastname =''
                    pname =''
                    pfirstname = au.find('first_name').text
                    pmiddlename = au.find('middle_name').text
                    plastname = au.find('last_name').text
                    if pfirstname is not None:
                        pname = str(pfirstname)
                    if pmiddlename is not None:
                        pname = pname + " " + str(pmiddlename)
                    if plastname is not None:
                        pname = pname + " " + str(plastname)
                    for aff in au.findall('affiliation'):
                        affiliation=aff.text
                        if affiliation is not None:
                            paffiliation = str(affiliation);
                        else:
                            paffiliation = "Empty value";
                        paper_content = []
                        paper_content.append(pdate)
                        paper_content.append(pname)
                        paper_content.append(paffiliation)
                        csvwriter.writerow(paper_content)
    Paper_data.close()


import csv
import os

working_path = 'D:\\ACM\\'
csv_file = 'DQDs - Listed relevant attributes - ACM.csv'
XML_path1 = 'D:\\ACM\\XML_files\\periodicals\\'
XML_path2 = 'D:\\ACM\\XML_files\\proceedings\\'

os.chdir(working_path)

Paper_data = open(csv_file, 'w', newline='') 
csvwriter = csv.writer(Paper_data) 
paper_head = ['Date','Full name', 'Affiliation']
csvwriter.writerow(paper_head)
Paper_data.close()

global file_count
file_count = 0

for root, dirs, files in os.walk(XML_path1):
    for file in files:
        subfolder=os.path.splitext(file)[0]
        XML_open_file(XML_path1+subfolder+'\\'+file,csv_file)

for root, dirs, files in os.walk(XML_path2):
    for file in files:
        subfolder=os.path.splitext(file)[0]
        XML_open_file(XML_path2+subfolder+'\\'+file,csv_file)
