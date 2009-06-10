AUTOMAKE_OPTIONS = subdir-objects

EJABBERD_PREFIX=$(libdir)/ejabberd

ESRCDIR=$(top_srcdir)/src
EBINDIR=$(top_builddir)/ebin

ERL=erl $(ERLCFLAGS) -pa ebin $(ERLFLAGS) -noshell -eval

mod_ejabberd_couch_FILES = ejabberd_couch.erl ejabberd_auth_couchdb.erl auth_couchdb_test.erl 
mod_ejabberd_couch_BEAMS = ejabberd_couch.beam ejabberd_auth_couchdb.beam auth_couchdb_test.beam

EXTRA_DIST = ${addprefix $(ESRCDIR)/, $(mod_ejabberd_couch_FILES)}

mod_ejabberd_couchdir = $(EJABBERD_PREFIX)/ebin
mod_ejabberd_couch_DATA = ${addprefix $(EBINDIR)/, $(mod_ejabberd_couch_BEAMS)}
CLEANFILES = $(mod_ejabberd_couch_DATA)

test: all
	$(ERL) 'auth_couchdb_test:test(), halt().'

$(EBINDIR)/%.beam: $(ESRCDIR)/%.erl
	$(ERLC) $(ERLCFLAGS) -b beam -o $(EBINDIR) $<
