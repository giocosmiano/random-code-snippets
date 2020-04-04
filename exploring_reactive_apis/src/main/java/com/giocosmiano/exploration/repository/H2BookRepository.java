package com.giocosmiano.exploration.repository;

import com.giocosmiano.exploration.domain.H2Book;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface H2BookRepository extends
        CrudRepository<H2Book, Long> {

    List<H2Book> findByIsbn(String id);
}
