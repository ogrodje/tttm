create table tournaments
(
    id             uuid                            not null
        constraint tournaments_pk primary key,
    players_config jsonb       default '{}'::jsonb not null,
    size_3         jsonb       default '[]'::jsonb,
    size_5         jsonb       default '[]'::jsonb,
    size_7         jsonb       default '[]'::jsonb,
    created_at     timestamptz default now(),
    updated_at     timestamptz default now()
);

