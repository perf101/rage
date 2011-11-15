create table tc_migratedowntime (
	job_id	integer not null,
	build	varchar(32) not null,
	vmimage	varchar(64) not null,
	primary key (job_id)
);

insert into tc_migratedowntime values ('226860', 'trunk-51590', 'winxpsp3-pv-nofirewall.img');

create table results (
	som_id	integer not null,
	job_id	integer not null,
	result_id	integer not null,
	result double not null,
	primary key (som_id, job_id, result_id)
);
