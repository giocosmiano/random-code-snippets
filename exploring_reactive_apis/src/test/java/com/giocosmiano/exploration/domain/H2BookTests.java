package com.giocosmiano.exploration.domain;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Date;

import static org.assertj.core.api.Assertions.assertThat;

public class H2BookTests {

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
        H2Book h2Book = new H2Book();
        h2Book.setId(i);
        h2Book.setTitle("title" + i);
        h2Book.setIsbn("isbn" + i);
        h2Book.setPageCount(i * 10);
        h2Book.setPublishedDate(date);
        h2Book.setThumbnailUrl("thumbnailUrl" + i);
        h2Book.setShortDescription("shortDescription" + i);
        h2Book.setLongDescription("longDescription" + i);
        h2Book.setStatus("status" + i);
        h2Book.setAuthors(new ArrayList<>());
        h2Book.setCategories(new ArrayList<>());

        assertThat(h2Book.getId()).isEqualTo(i);
        assertThat(h2Book.getTitle()).isEqualTo("title" + i);
        assertThat(h2Book.getPageCount()).isEqualTo(i * 10);
        assertThat(h2Book.getPublishedDate()).isEqualTo(date);
    }
}