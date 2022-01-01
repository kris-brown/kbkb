DROP TABLE IF EXISTS img;
DROP TABLE IF EXISTS latex;
DROP TABLE IF EXISTS intlink;
DROP TABLE IF EXISTS content;
DROP TABLE IF EXISTS contents;
DROP TABLE IF EXISTS sections CASCADE;
DROP TABLE IF EXISTS comments CASCADE;
DROP TABLE IF EXISTS section CASCADE;
DROP TYPE IF EXISTS tag_type;


CREATE TYPE tag_type AS ENUM (
    'Default', 'Prop', 'Exercise', 'Def', 'Kris', 'Example');

-- Sum type: sections + contents (exactly one should have fk to each content elem)
CREATE TABLE section (
	id BIGINT PRIMARY KEY,
	parent BIGINT NOT NULL REFERENCES section(id) ON DELETE CASCADE ,
	ord SERIAL NOT NULL,
	n_children BIGINT,
	UNIQUE (id, ord)
);


CREATE TABLE sections (
	id BIGINT PRIMARY KEY,
	sect BIGINT NOT NULL REFERENCES section(id) ON DELETE CASCADE,
    tag tag_type NOT NULL,
    title TEXT NOT NULL,
    uuid TEXT NOT NULL UNIQUE
);


CREATE TABLE contents (
    id BIGINT PRIMARY KEY,
    filetype TEXT NOT NULL,
	sect BIGINT NOT NULL REFERENCES section(id) ON DELETE CASCADE
);

CREATE TABLE comments (
	id SERIAL PRIMARY KEY,
	sect BIGINT NOT NULL REFERENCES sections(id) ON DELETE CASCADE,
	email TEXT NOT NULL,
	tstamp TEXT NOT NULL,
	body TEXT NOT NULL
);

-- Sum type: IntLink + LaTeX (exactly one should have fk to each content elem)
CREATE TABLE content (
    id SERIAL PRIMARY KEY,
	cont BIGINT NOT NULL REFERENCES contents(id) ON DELETE CASCADE,
	ord SERIAL NOT NULL,
	UNIQUE (id, ord)
);

CREATE TABLE latex (
    id SERIAL PRIMARY KEY,
    cont BIGINT NOT NULL REFERENCES content(id) ON DELETE CASCADE,
    val TEXT NOT NULL
);

CREATE TABLE intlink (
    id SERIAL PRIMARY KEY,
    uuid TEXT NOT NULL, -- implicitly references SECTION.uid
    cont BIGINT NOT NULL REFERENCES content(id) ON DELETE CASCADE,
    display TEXT NOT NULL,
    comm TEXT
);

CREATE TABLE img (
	pth TEXT PRIMARY KEY,
	val TEXT NOT NULL,
	caption TEXT
);
