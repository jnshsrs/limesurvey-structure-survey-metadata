# -*- coding: utf-8 -*-

import xml.etree.ElementTree as ET
import pymysql
from collections import Counter
import sqlite3
from bs4 import BeautifulSoup
import re

# path = 'C:/Users/ZeMIT_/Documents/R/IT-Report-2016/'
# xml_file_name = './pdl-survey-structure-pretest.xml'
# database_name = './pdl-survey-structure-it-report-16.sqlite'

xml_file_name = './cio-survey-structure-pretest.xml'
database_name = './cio-survey-structure-it-report-16-backup-2.sqlite'


dom = ET.parse(xml_file_name)
root = dom.getroot()

conn=sqlite3.connect(database_name)
cur = conn.cursor()

cur.execute('PRAGMA encoding = "UTF-8";')
conn.commit()
# Create table
cur.execute('''CREATE TABLE section
             (id INTEGER PRIMARY KEY ASC, 
              section_internal_id TEXT charset utf8,
              section_title TEXT charset utf8)''')
              
cur.execute('''CREATE TABLE IF NOT EXISTS question
             (id INTEGER PRIMARY KEY ASC, 
              question_internal_id TEXT charset utf8,
              section_id TEXT charset utf8,
              question_text TEXT charset utf8,
              question_type TEXT charset utf8)''')
              
cur.execute('''CREATE TABLE IF NOT EXISTS response
                (id INTEGER PRIMARY KEY ASC,
                 response_label TEXT charset utf8,
                 response_value TEXT charset utf8)''')
                
cur.execute('''CREATE TABLE IF NOT EXISTS question_response
                (question_id INT,
                 response_id INT);''')

cur.execute('''CREATE TABLE IF NOT EXISTS subquestion
                (id INTEGER PRIMARY KEY ASC,
                 subquestion_internal_id TEXT charset utf8,
                 section_id INT DEFAULT NULL,
                 question_id INT,
                 subquestion_text TEXT charset utf8)''')

cur.execute('''CREATE TABLE IF NOT EXISTS format
                (id INTEGER PRIMARY KEY ASC,
                 format_type TEXT DEFAULT NULL,
                 format_length INT DEFAULT NULL)''')
                 
cur.execute('''CREATE TABLE IF NOT EXISTS question_format
                (question_id INTEGER,
                 format_id INTEGER)''')

conn.commit()
conn.close()

insert_section = '''INSERT INTO section (section_internal_id, section_title) 
                VALUES (%s, '%s')'''
                
insert_question = '''INSERT INTO question (question_internal_id, section_id, question_text, question_type)
                    VALUES ('%s', %s, '%s', '%s')'''

insert_response = '''INSERT INTO response (response_label, response_value)
                    VALUES ("%s", "%s")'''

insert_subquestion = '''INSERT INTO subquestion (subquestion_internal_id, section_id, question_id, subquestion_text)
                    VALUES ("%s", "%s", "%s", "%s")'''
                    
insert_format = '''INSERT INTO format (format_type, format_length)
                    VALUES ("%s", "%s")'''
                    
insert_question_format = '''INSERT INTO question_format (question_id, format_id)
                    VALUES ("%s", "%s")'''

conn = sqlite3.connect(database_name)
cur = conn.cursor()
for section in root.iterfind(".//section"):
    # section id
    section_internal_id = section.attrib['id']
    # section title
    section_title = section.find("./sectionInfo/text").text
    # test if value is already in db
    cur.execute("select section_internal_id from section where section_internal_id = %s;" % section_internal_id)
    qry_result_section = cur.fetchall()
    if not qry_result_section: 
        cur.execute(insert_section % (section_internal_id, section_title))
    cur.execute("select id from section where section_internal_id = %s;" % section_internal_id)
    section_id = cur.fetchall()[0][0]
    # iter over questions
    for question in section.iterfind('.//question'):
        # get values from xml
        response = question.findall('.//response')
        for r in response:
            try: 
                question_internal_id = r.attrib['varName']
            except:
                pass
        question_text = question.find('./text').text
        question_text = question_text.encode(encoding='UTF-8',errors='strict')
        question_text = re.sub('|', '', question_text)
        question_text = BeautifulSoup(question_text, 'lxml').text
        question_text = re.sub('<[^<]+?>', '', question_text)
        question_type = question.find('.//response/*[1]').tag

        # insert values into sql db
        # insert questions
        cur.execute("SELECT question_internal_id FROM question WHERE question_internal_id = '%s'" % question_internal_id)
        qry_result_question = cur.fetchall()
        if not qry_result_question:
            cur.execute(insert_question % (question_internal_id, section_id, question_text, question_type))
            conn.commit()
        cur.execute("SELECT id FROM question WHERE question_internal_id = '%s'" % question_internal_id)
        question_id = cur.fetchall()[0][0]
        # subquestion
        for subquestion in question.iterfind('.//subQuestion'):
            subquestion_internal_id = subquestion.attrib['varName']
            subquestion_text = subquestion.find('./text').text.replace('"', "'")
            subquestion_text = BeautifulSoup(subquestion_text, 'lxml').text
            subquestion_text = re.sub('<[^<]+?>', '', subquestion_text)
            # check if subquestion is already in database
            cur.execute("select section_internal_id from section where section_internal_id = '%s';" % subquestion_internal_id)
            internal_id = cur.fetchall()
            if not internal_id: 
                cur.execute(insert_subquestion % (subquestion_internal_id, section_id, question_id, subquestion_text))
            cur.execute('''SELECT id FROM subquestion WHERE subquestion_internal_id = "%s"''' % subquestion_internal_id)
            subquestion_id = cur.fetchall()[0][0]
        # response table
        if question_type == 'fixed':
            for category in question.iterfind('.//category'):
                question_label = category.find('.//label').text
                question_value = category.find('.//value').text
                # insert response values
                cur.execute('''SELECT id FROM response WHERE response_label = "%s" AND response_value = "%s";''' % (question_label, question_value))
                qry_result_response = cur.fetchall()
                if not qry_result_response:
                    cur.execute(insert_response % (question_label, question_value))
                cur.execute('''SELECT id FROM response WHERE  response_label = "%s" AND response_value = "%s";''' % (question_label, question_value))
                response_id = cur.fetchall()[0][0]
                cur.execute('''INSERT INTO question_response VALUES (%s, %s)''' % (question_id, response_id))
        elif question_type == "free":
                format_format = question.find('.//response/free/format').text
                format_length =  question.find('.//response/free/length').text
                cur.execute('''SELECT id FROM format WHERE format_type = "%s" AND format_length = "%s";''' % (format_format, format_length))
                qry_result_format = cur.fetchall()
                if not qry_result_format:
                    cur.execute(insert_format % (format_format, format_length))
                cur.execute('''SELECT id FROM format WHERE format_type = "%s" AND format_length = "%s";''' % (format_format, format_length))
                format_id = cur.fetchall()[0][0]
                cur.execute('''INSERT INTO question_format VALUES (%s, %s)''' % (question_id, format_id))
        else:
            print "Unknown response type"


conn.commit()
conn.close()