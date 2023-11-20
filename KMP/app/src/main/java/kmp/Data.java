package kmp;

public class Data {
    private String testName;
    private int id;
    private long duration;

    public long getDuration() {
        return duration / 1000000; // Accesseur de lecture
    }

    public String getTestName() {
        return testName;
    }

    public int getId() {
        return id; // Accesseur de lecture
    }

    Data(long StartTime, long EndTime, int id, String testName) {
        this.duration = EndTime - StartTime;
        this.id = id;
        this.testName = testName;
    }

    public Data() {
    }

}
