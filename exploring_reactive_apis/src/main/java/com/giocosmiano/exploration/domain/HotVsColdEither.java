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
}