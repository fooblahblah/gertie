#!/bin/sh

OUTPUT_FILE="gertie-`date +'%s'`"

find target/ -name "*-one-jar.jar" -delete

sbt one-jar

cat << 'EOF' > $OUTPUT_FILE
#!/bin/sh
MYSELF=`which "$0" 2>/dev/null`
[ $? -gt 0 -a -f "$0" ] && MYSELF="./$0"
java=java
if test -n "$JAVA_HOME"; then
    java="$JAVA_HOME/bin/java"
fi
exec "$java" $java_args -jar $MYSELF "$@"
exit 1
EOF

find target/ -name "*-one-jar.jar" -exec cat {} ';' >> $OUTPUT_FILE

chmod +x $OUTPUT_FILE
ln -sf $OUTPUT_FILE gertie