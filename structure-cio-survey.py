import xml.etree.ElementTree as ET
import pymysql
from collections import Counter
import sqlite3

path = 'C:/Users/ZeMIT_/Documents/R/IT-Report-2016/'
xml_file_name = 'cio-survey-structure-pretest.xml'
database_name = "cio-survey-structure-it-report-16.sqlite"

dom = ET.parse(path + xml_file_name)
root = dom.getroot()

conn=sqlite3.connect(path + database_name)
cur = conn.cursor()

# Create table
cur.execute('''CREATE TABLE section
             (id INTEGER PRIMARY KEY ASC, 
              internal_id TEXT,
              title TEXT)''')
              
cur.execute('''CREATE TABLE IF NOT EXISTS question
             (id INTEGER PRIMARY KEY ASC, 
              internal_id TEXT,
              section_id TEXT,
              text TEXT,
              type TEXT)''')
              
cur.execute('''CREATE TABLE IF NOT EXISTS response
                (id INTEGER PRIMARY KEY ASC,
                 label TEXT,
                 value TEXT)''')
                
cur.execute('''CREATE TABLE IF NOT EXISTS question_response
                (question_id INT,
                 response_id INT);''')

cur.execute('''CREATE TABLE IF NOT EXISTS subquestion
                (id INTEGER PRIMARY KEY ASC,
                 internal_id TEXT,
                 section_id INT DEFAULT NULL,
                 question_id INT,
                 text TEXT)''')

cur.execute('''CREATE TABLE IF NOT EXISTS format
                (id INTEGER PRIMARY KEY ASC,
                 format TEXT DEFAULT NULL,
                 length INT DEFAULT NULL)''')
                 
cur.execute('''CREATE TABLE IF NOT EXISTS question_format
                (question_id INTEGER,
                 format_id INTEGER)''')

conn.commit()
conn.close()

insert_section = '''INSERT INTO section (internal_id, title) 
                VALUES (%s, '%s')'''
                
insert_question = '''INSERT INTO question (internal_id, section_id, text, type)
                    VALUES ('%s', %s, '%s', '%s')'''

insert_response = '''INSERT INTO response (label, value)
                    VALUES ("%s", "%s")'''

insert_subquestion = '''INSERT INTO subquestion (internal_id, section_id, question_id, text)
                    VALUES ("%s", "%s", "%s", "%s")'''
                    
insert_format = '''INSERT INTO format (format, length)
                    VALUES ("%s", "%s")'''
                    
insert_question_format = '''INSERT INTO question_format (question_id, format_id)
                    VALUES ("%s", "%s")'''


conn=sqlite3.connect(path + database_name)
cur = conn.cursor()
for section in root.iterfind(".//section"):
    # section id
    section_internal_id = section.attrib['id']
    # section title
    section_title = section.find("./sectionInfo/text").text
    # test if value is already in db
    cur.execute("select internal_id from section where internal_id = %s;" % section_internal_id)
    qry_result_section = cur.fetchall()
    if not qry_result_section: 
        cur.execute(insert_section % (section_internal_id, section_title))
    cur.execute("select id from section where internal_id = %s;" % section_internal_id)
    section_id = cur.fetchall()[0][0]
    # iter over questions
    for question in section.iterfind('.//question'):
        # get values from xml
        question_internal_id = question.find('.//response').attrib['varName']
        question_text = question.find('./text').text
        question_type = question.find('.//response/*[1]').tag
        # insert values into sql db
        # insert questions
        cur.execute("SELECT internal_id FROM question WHERE internal_id = '%s'" % question_internal_id)
        qry_result_question = cur.fetchall()
        if not qry_result_question:
            cur.execute(insert_question % (question_internal_id, section_id, question_text, question_type))
            conn.commit()
        cur.execute("SELECT id FROM question WHERE internal_id = '%s'" % question_internal_id)
        question_id = cur.fetchall()[0][0]
        # subquestion
        for subquestion in question.iterfind('.//subQuestion'):
            subquestion_internal_id = subquestion.attrib['varName']
            subquestion_text = subquestion.find('./text').text.replace('"', "'")
            # check if subquestion is already in database
            cur.execute("select internal_id from section where internal_id = '%s';" % subquestion_internal_id)
            internal_id = cur.fetchall()
            if not internal_id: 
                cur.execute(insert_subquestion % (subquestion_internal_id, section_id, question_id, subquestion_text))
            cur.execute('''SELECT id FROM subquestion WHERE internal_id = "%s"''' % subquestion_internal_id)
            subquestion_id = cur.fetchall()[0][0]
        # response table
        if question_type == 'fixed':
            for category in question.iterfind('.//category'):
                question_label = category.find('.//label').text
                question_value = category.find('.//value').text
                # insert response values
                cur.execute('''SELECT id FROM response WHERE label = "%s" AND value = "%s";''' % (question_label, question_value))
                qry_result_response = cur.fetchall()
                if not qry_result_response:
                    cur.execute(insert_response % (question_label, question_value))
                cur.execute('''SELECT id FROM response WHERE  label = "%s" AND value = "%s";''' % (question_label, question_value))
                response_id = cur.fetchall()[0][0]
                cur.execute('''INSERT INTO question_response VALUES (%s, %s)''' % (question_id, response_id))
        elif question_type == "free":
                format_format = question.find('.//response/free/format').text
                format_length =  question.find('.//response/free/length').text
                cur.execute('''SELECT id FROM format WHERE format = "%s" AND length = "%s";''' % (format_format, format_length))
                qry_result_format = cur.fetchall()
                if not qry_result_format:
                    cur.execute(insert_format % (format_format, format_length))
                cur.execute('''SELECT id FROM format WHERE format = "%s" AND length = "%s";''' % (format_format, format_length))
                format_id = cur.fetchall()[0][0]
                cur.execute('''INSERT INTO question_format VALUES (%s, %s)''' % (question_id, format_id))
        else:
            print "Unknown response type"


conn.commit()
conn.close()








