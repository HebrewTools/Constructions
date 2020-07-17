from camilstaps/clean:nightly as builder

copy . /app/construction-searcher
workdir /app/construction-searcher

run dpkg --add-architecture i386 && \
	apt-get update -qq && \
	apt-get install -qq gcc-multilib && \
	CLEAN_PLATFORM=x86 install_clean_nightly.sh \
		abc-interpreter \
		base \
		lib-dynamics \
		lib-graphcopy \
		lib-itasks \
		lib-platform \
		lib-tcpip && \
	tail -n +3 /opt/clean/etc/iTasks.env | sed 's/cocl-itasks:/cocl-itasks:-h 1000m /' >> /opt/clean/etc/IDEEnvs && \
	cp search.prj.default search.prj && \
	cp server.prj.default server.prj && \
	cpm search.prj && \
	cpm server.prj

from debian:buster-slim AS runner

run dpkg --add-architecture i386 && \
	apt-get update -qq && \
	apt-get install -qq libc6-i386 curl

run mkdir -p /root/text-fabric-data/etcbc/bhsa/tf/c && \
	for f in book chapter g_word_utf8 gn lex ls nu otype oslots pdp ps sp st trailer_utf8 verse vs vt; do \
		curl https://raw.githubusercontent.com/ETCBC/bhsa/master/tf/c/$f.tf -o /root/text-fabric-data/etcbc/bhsa/tf/c/$f.tf; \
	done

workdir /app/construction-searcher
copy --from=builder /app/construction-searcher/search /app/construction-searcher/search
copy --from=builder /app/construction-searcher/server /app/construction-searcher/server
copy --from=builder /app/construction-searcher/server.bc /app/construction-searcher/server.bc
copy --from=builder /app/construction-searcher/server.pbc /app/construction-searcher/server.pbc
copy --from=builder /app/construction-searcher/server-www /app/construction-searcher/server-www

expose 8080

entrypoint ["./server", "--allowed-hosts=0.0.0.0"]
cmd []
