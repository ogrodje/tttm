SELECT tournament_id,
       day,
       (day_scores ->> 'id')                                                      AS server_id,
       (day_scores -> 'score')                                                    AS score,
       row_number() OVER (PARTITION BY day ORDER BY (day_scores -> 'score') DESC) AS day_ranking
FROM (SELECT t.created_at::timestamp::date AS day,
             jsonb_array_elements(size_3_scores)  as day_scores,
             t.id                          as tournament_id
      FROM tournaments t
      WHERE
          -- t.created_at::timestamp::date >= CURRENT_DATE - '30 days'::interval
          t.id = 'fbeb90fa-de12-474c-ae50-b90bc1624fbc'::uuid
      GROUP BY t.created_at::timestamp::date,
               size_3_scores, t.id
      ORDER BY t.created_at::timestamp::date DESC,
               size_3_scores -> 'score' DESC) AS day_scores

-- WHERE tournament_id = 'fbeb90fa-de12-474c-ae50-b90bc1624fbc'::uuid
-- ORDER BY day DESC

;

