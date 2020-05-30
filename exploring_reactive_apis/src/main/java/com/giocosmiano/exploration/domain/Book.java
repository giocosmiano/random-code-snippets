package com.giocosmiano.exploration.domain;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

@ToString
@AllArgsConstructor
@NoArgsConstructor
@Data
@Document(collection = "books")
public class Book implements Serializable {

    private static final long serialVersionUID = 6614729067517057698L;

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

//  Using Lombok @NoArgsConstructor to take care of this
//    public Book() {}

/*
 * Using Lombok @AllArgsConstructor to take care of these
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
*/
}

