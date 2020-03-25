package com.giocosmiano.exploration.domain;

import lombok.Data;
import org.joda.time.DateTime;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@Data
@Document
public class Book {

    @Id
    private String id;
    private String origId;
    private String title;
    private String isbn;
    private Long pageCount;
    private String publishedDateStr;
    private DateTime publishedDate;
    private String thumbnailUrl;
    private String shortDescription;
    private String longDescription;
    private String status;
    private List<String> authors;
    private List<String> categories;

    public Book() {}

    public Book(String id, String origId, String title, String isbn, Long pageCount
            , String publishedDateStr, DateTime publishedDate, String thumbnailUrl
            , String shortDescription, String longDescription, String status
            , List<String> authors, List<String> categories) {
        this.id = id;
        this.origId = origId;
        this.title = title;
        this.isbn = isbn;
        this.pageCount = pageCount;
        this.publishedDateStr = publishedDateStr;
        this.publishedDate = publishedDate;
        this.thumbnailUrl = thumbnailUrl;
        this.shortDescription = shortDescription;
        this.longDescription = longDescription;
        this.status = status;
        this.authors = authors;
        this.categories = categories;
    }
}

