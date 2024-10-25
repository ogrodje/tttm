alter table tournaments
    add size_3_scores jsonb default '[]'::jsonb;

alter table tournaments
    add size_5_scores jsonb default '[]'::jsonb;

alter table tournaments
    add size_7_scores jsonb default '[]'::jsonb;
