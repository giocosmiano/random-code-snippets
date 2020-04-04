package com.giocosmiano.exploration.domain;

import lombok.*;

import javax.persistence.*;
import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

@Data // combines @Getter, @Setter, @ToString, @EqualsAndHashCode, @RequiredArgsConstructor
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = "h2Books")
public class H2Book implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy= GenerationType.AUTO)
    private Long id;
    private String title;
    private String isbn;
    private Long pageCount;
    private Date publishedDate;
    private String thumbnailUrl;

    @Column(name="shortDescription", columnDefinition="LONGTEXT")
    private String shortDescription;

    @Column(name="longDescription", columnDefinition="LONGTEXT")
    private String longDescription;

    private String status;
    private String authors0;
    private String authors1;
    private String authors2;
    private String authors3;
    private String authors4;
    private String authors5;
    private String authors6;
    private String authors7;
    private String categories0;
    private String categories1;
    private String categories2;
    private String categories3;

    /* NOT exposing these getters for individual author/category items */
    private String getAuthors0() {
        return authors0;
    }

    private String getAuthors1() {
        return authors1;
    }

    private String getAuthors2() {
        return authors2;
    }

    private String getAuthors3() {
        return authors3;
    }

    private String getAuthors4() {
        return authors4;
    }

    private String getAuthors5() {
        return authors5;
    }

    private String getAuthors6() {
        return authors6;
    }

    private String getAuthors7() {
        return authors7;
    }

    private String getCategories0() {
        return categories0;
    }

    private String getCategories1() {
        return categories1;
    }

    private String getCategories2() {
        return categories2;
    }

    private String getCategories3() {
        return categories3;
    }

    public void setAuthors(final List<String> authors) {
        if (Objects.nonNull(authors) && authors.size() > 0) {
            authors0 = authors.get(0);

            if (authors.size() > 1) {
                authors1 = authors.get(1);
            }
            if (authors.size() > 2) {
                authors2 = authors.get(2);
            }
            if (authors.size() > 3) {
                authors3 = authors.get(3);
            }
            if (authors.size() > 4) {
                authors4 = authors.get(4);
            }
            if (authors.size() > 5) {
                authors5 = authors.get(5);
            }
            if (authors.size() > 6) {
                authors6 = authors.get(6);
            }
            if (authors.size() > 7) {
                authors7 = authors.get(7);
            }
        }
    }

    @Transient
    public Collection<String> getAuthors() {
        return Arrays.asList(
                authors0
                , authors1
                , authors2
                , authors3
                , authors4
                , authors5
                , authors6
                , authors7
        )
                .stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toList())
                ;
    }

    public void setCategories(final List<String> categories) {
        if (Objects.nonNull(categories) && categories.size() > 0) {
            categories0 = categories.get(0);

            if (categories.size() > 1) {
                categories1 = categories.get(1);
            }
            if (categories.size() > 2) {
                categories2 = categories.get(2);
            }
            if (categories.size() > 3) {
                categories3 = categories.get(3);
            }
        }
    }

    @Transient
    public Collection<String> getCategories() {
        return Arrays.asList(
                categories0
                , categories1
                , categories2
                , categories3
        )
                .stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toList())
                ;
    }
}

