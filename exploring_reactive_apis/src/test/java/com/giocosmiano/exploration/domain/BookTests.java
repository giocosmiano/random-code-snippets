package com.giocosmiano.exploration.domain;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Date;

import static org.assertj.core.api.Assertions.assertThat;

public class BookTests {

    /*
     https://stackoverflow.com/questions/41161076/adding-lombok-plugin-to-intellij-project
     To make Annotation work, such as Lombok, in IntelliJ
     1) Install Lombok plugin
     2) Go to Settings, Build/Execution/Deployment, Compiler, Enable `Annotation Processors`
     */
    @Test
    public void booksManagedByLombokShouldWork() {
        long i = 1;
        Date date = new Date();
        Book book =
                new Book(i
                        , "title" + i
                        , "isbn" + i
                        , i * 10
                        , date
                        , "thumbnailUrl" + i
                        , "shortDescription" + i
                        , "longDescription" + i
                        , "status" + i
                        , new ArrayList<>()
                        , new ArrayList<>()
                );

        assertThat(book.getId()).isEqualTo(i);
        assertThat(book.getTitle()).isEqualTo("title" + i);
        assertThat(book.getPageCount()).isEqualTo(i * 10);
        assertThat(book.getPublishedDate()).isEqualTo(date);
    }
}