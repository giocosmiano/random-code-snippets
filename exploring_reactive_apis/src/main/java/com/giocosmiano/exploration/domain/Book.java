package com.giocosmiano.exploration.domain;

import lombok.Data;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.Date;
import java.util.List;

@Data
@Document
public class Book {

    @Id
    private Long id;
    private String title;
    private String isbn;
    private Long pageCount;
    private Date publishedDate;
    private String thumbnailUrl;
    private String shortDescription;
    private String longDescription;
    private String status;
    private List<String> authors;
    private List<String> categories;

    public Book() {}

    public Book(Long id, String title, String isbn, Long pageCount
            , Date publishedDate, String thumbnailUrl
            , String shortDescription, String longDescription, String status
            , List<String> authors, List<String> categories) {
        this.id = id;
        this.title = title;
        this.isbn = isbn;
        this.pageCount = pageCount;
        this.publishedDate = publishedDate;
        this.thumbnailUrl = thumbnailUrl;
        this.shortDescription = shortDescription;
        this.longDescription = longDescription;
        this.status = status;
        this.authors = authors;
        this.categories = categories;
    }
}

