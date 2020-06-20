--Edge list no duplicates
SELECT 
	x.relocation_id, x.name AS benefit_name
FROM OPENQUERY(POINTC, '
	SELECT 
		rb.relocation_id,
		b.name,
		r.external_id
		--count(*) AS cnt 
	FROM 
		public.relocation_benefits AS rb
		INNER JOIN public.benefits b ON b.id = rb.benefit_id
		INNER JOIN public.relocations r on r.id = rb.relocation_id
	WHERE 
		rb.state = ''redeemed''
	GROUP BY 
		rb.relocation_id, b.name, r.external_id;') AS x
	INNER JOIN Movetrack.dbo.Authorizations a ON a.AuthorizationID = x.external_id
	ORDER BY x.relocation_id, x.name

--Edge list duplicates
SELECT 
	x.relocation_id, x.name AS benefit_name
FROM OPENQUERY(POINTC, '
	SELECT 
		rb.relocation_id,
		b.name,
		r.external_id
		--count(*) AS cnt 
	FROM 
		public.relocation_benefits AS rb
		INNER JOIN public.benefits b ON b.id = rb.benefit_id
		INNER JOIN public.relocations r on r.id = rb.relocation_id
	WHERE 
		rb.state = ''redeemed'';') AS x
	INNER JOIN Movetrack.dbo.Authorizations a ON a.AuthorizationID = x.external_id
	ORDER BY x.relocation_id, x.name

--Vertex attributes
SELECT x.*, lvl.level, a.OldState AS 'from_state', a.NewState AS 'to_state'
FROM OPENQUERY(POINTC, '
	SELECT 
		r.id AS relocation_id, 
		r.external_id,
		MAX(spouse) AS spouse, 
		MAX(child) AS child, 
		MAX(parent) AS parent, 
		MAX(pet) AS pet,
		MAX(vehicle) AS vehicle,
		MAX(homeowner) AS homeowner,
		MAX(homebuyer) AS homebuyer
	FROM public.relocations r
	LEFT JOIN 
		(
		SELECT 
			qa.relocation_id,
			CASE WHEN qao.text = ''My spouse or partner'' THEN 1 ELSE 0 END AS spouse,
			CASE WHEN qao.text = ''My child(ren)'' THEN 1 ELSE 0 END AS child,
			CASE WHEN qao.text = ''My parent(s), grandparent(s), or other adults'' THEN 1 ELSE 0 END AS parent,
			CASE WHEN q.text = ''Are you moving with pets?'' AND qao.text = ''Yes'' THEN 1 ELSE 0 END AS pet,
			CASE WHEN q.text = ''Will you move with vehicles (e.g. car, boat)?'' AND qao.text = ''Yes'' THEN 1 ELSE 0 END as vehicle,
			CASE WHEN q.text = ''Do you rent or own your current residence?'' AND qao.text = ''Own'' THEN 1 ELSE 0 END as homeowner,
			CASE WHEN q.text = ''Do you plan to rent or buy your new residence?'' AND qao.text = ''Buy'' THEN 1 ELSE 0 END as homebuyer		
		FROM 
			public.question_answers AS qa
			INNER JOIN public.question_answer_options qao ON qao.id = qa.question_answer_option_id
			INNER JOIN public.questions q on q.id = qa.question_id
		) AS x ON x.relocation_id = r.id
	GROUP BY 
		r.id, r.external_id;
	') AS x
	INNER JOIN Movetrack.dbo.Authorizations a ON a.AuthorizationID = x.external_id
	INNER JOIN
		(
		SELECT 
			x.ReloPolicy, 
			RANK() OVER(ORDER BY x.monetary_value) AS level
		FROM 
			(
			SELECT 
				a.ReloPolicy, 
				MAX(x.monetary_value) AS monetary_value 
			FROM OPENQUERY(POINTC, '
				SELECT 
					external_id, 
					monetary_value 
				FROM
					public.relocations
				') AS x
				INNER JOIN Movetrack.dbo.Authorizations a 
				ON a.AuthorizationID = x.external_id
			WHERE 
				a.ReloPolicy not in ('', 'LS + HHG Mgd Move')
			GROUP BY 
				a.ReloPolicy
			) AS x
		) AS lvl on lvl.ReloPolicy = a.ReloPolicy
	INNER JOIN (SELECT * FROM OPENQUERY(POINTC, 'SELECT DISTINCT rb.relocation_id FROM public.relocation_benefits AS rb WHERE rb.state = ''redeemed'';')) AS x2 ON x2.relocation_id = x.relocation_id 
	ORDER BY relocation_id

--Move members bipartite
select * from OPENQUERY(POINTC, '
	select qa.relocation_id,
	CASE WHEN qao.text = ''Yes'' THEN ''Pet'' ELSE qao.text END
	from public.question_answers qa
	inner join public.question_answer_options qao 
	on qao.id = qa.question_answer_option_id
	where qa.question_id in (3, 4)
	and qao.text != ''Just me, myself, and I''
	and qao.text != ''No''
	order by qa.relocation_id, qao.id
')
UNION ALL
select * from OPENQUERY(POINTC, '
	select qa.relocation_id,
	qao.text
	from public.question_answers qa
	inner join public.question_answer_options qao 
	on qao.id = qa.question_answer_option_id
	inner join
		(
			select qa.relocation_id
			from public.question_answers qa
			where qa.question_id in (3)
			group by qa.relocation_id
			having count(*) = 1
		) x on x.relocation_id = qa.relocation_id
	where qa.question_id in (3)
	and qao.text = ''Just me, myself, and I''
	order by qa.relocation_id, qao.id
')