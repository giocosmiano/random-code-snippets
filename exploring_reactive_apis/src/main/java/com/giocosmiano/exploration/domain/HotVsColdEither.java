package com.giocosmiano.exploration.domain;

import lombok.Data;

@Data
public class HotVsColdEither {

    private Integer rightValue;
    private String leftValue;

    public HotVsColdEither() {}

    public HotVsColdEither(Integer rightValue, String leftValue) {
        this.rightValue = rightValue;
        this.leftValue = leftValue;
    }

    public Integer getRightValue() {
        return rightValue;
    }

    public void setRightValue(Integer rightValue) {
        this.rightValue = rightValue;
    }

    public String getLeftValue() {
        return leftValue;
    }

    public void setLeftValue(String leftValue) {
        this.leftValue = leftValue;
    }
}