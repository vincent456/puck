
package p;

interface LogEntry {
    public LogEntry clone();
}

abstract class BaseEntry implements LogEntry {
    @Override
    public LogEntry clone(){
        try{
            return (LogEntry)super.clone();
        } catch (CloneNotSupportedException e) {
            throw new Error();
        }
    }
}

class INLogEntry extends BaseEntry implements LogEntry {}

/*
interface LogEntry {
    public LogEntry clone();
}
interface Loggable {}
interface LatchContext{}

abstract class Node implements Loggable {}
class IN  extends Node implements Comparable<IN>, LatchContext {
    public int compareTo(IN in){return 0;}
}

interface INContainingEntry {}

abstract class BaseEntry<T extends Loggable> implements LogEntry {
    @Override
    public LogEntry clone(){
        try{
            return (LogEntry)super.clone();
        } catch (CloneNotSupportedException e) {
            throw new Error();
        }
    }
}

class INLogEntry<T extends IN> extends BaseEntry<T> implements LogEntry, INContainingEntry {}*/
