DROP TABLE IF EXISTS comments;
DROP TABLE IF EXISTS link;
DROP TABLE IF EXISTS section;
DROP TABLE IF EXISTS img;

-- Sum type: sections + contents (exactly one should have fk to each content elem)
CREATE TABLE section (
	id BIGINT PRIMARY KEY,
	parent BIGINT NOT NULL REFERENCES section(id) ON DELETE CASCADE,
	ord SERIAL NOT NULL,
    uuid Text NOT NULL UNIQUE,
    title TEXT NOT NULL,
	n_children INT NOT NULL,
    tex TEXT,
    urlpth TEXT,
    html TEXT,
    pdf TEXT,
	UNIQUE (id, ord)
);

CREATE TABLE link (
	id SERIAL PRIMARY KEY,
	src BIGINT NOT NULL REFERENCES section(id),
	tgt BIGINT NOT NULL REFERENCES section(id),
    display Text NOT NULL,
	comm Text NOT NULL,
	repl Text NOT NULL
);

CREATE TABLE comments (
	id SERIAL PRIMARY KEY,
	sect BIGINT NOT NULL REFERENCES section(id),
	email TEXT NOT NULL,
	tstamp TEXT NOT NULL,
	body TEXT NOT NULL
);

CREATE TABLE img (
	id SERIAL PRIMARY KEY,
	iname TEXT NOT NULL,
	ival BYTEA
);