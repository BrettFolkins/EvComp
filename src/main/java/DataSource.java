package com;

public interface DataSource {
    /* Should provide all data between 0.0 and 1.0 on x*/
    public double get(double x);
    public String getName();
}
