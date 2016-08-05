-- survey structure with limesurvey IDs
select section.title as Kapitel, section.`internal_id` as KapitelID, t1.text as Frage, t1.internal_id as Frage, t1.`text:1` as Unterfrage, t1.`internal_id:1` as UnterfrageID from 
	(select * from question
		left join subquestion on question.id= subquestion.question_id) as t1
left join section on t1.section_id = section.id

-- questions and subquestions with there value and their labels
select * from (
	select * from (
		select * from (  
			select * from (
				select * 
				  from question 
				  left join subquestion on question.section_id = subquestion.id) as t1
		left join section on t1.section_id = section.id) as t2
	left join question_response on t2.id = question_response.question_id) as t3
left join response on response.id = t3.response_id) as t4
where text <> 'PRETEST KOMMENTAR';