#' Format Publications from CSV for CV
#' 
#' Reads publications from CSV and formats them according to CV style
#' 
#' @param csv_path Path to publications CSV file
#' @param type_filter Optional: filter by type (e.g., "paper", "report", "chapter", "software", "book")
#' @param year_filter Optional: filter by year
#' @return Character vector of formatted publication entries
format_publications <- function(csv_path = "docs/cv/publications.csv", 
                                type_filter = NULL,
                                year_filter = NULL) {
  
  # Read CSV
  pubs <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # Apply filters
  if (!is.null(type_filter)) {
    pubs <- pubs[pubs$type == type_filter, ]
  }
  if (!is.null(year_filter)) {
    pubs <- pubs[pubs$year == year_filter, ]
  }
  
  # Sort by year (descending), then by title
  pubs <- pubs[order(-as.numeric(pubs$year), pubs$title), ]
  
  # Format each publication
  formatted <- character(nrow(pubs))
  
  for (i in seq_len(nrow(pubs))) {
    p <- pubs[i, ]
    
    if (p$type == "paper") {
      formatted[i] <- format_paper(p)
    } else if (p$type == "report") {
      formatted[i] <- format_report(p)
    } else if (p$type == "chapter") {
      formatted[i] <- format_chapter(p)
    } else if (p$type == "software") {
      formatted[i] <- format_software(p)
    } else if (p$type == "book") {
      formatted[i] <- format_book(p)
    } else {
      # For working papers, under review, etc., format as paper
      formatted[i] <- format_paper(p)
    }
  }
  
  return(formatted)
}

#' Format a paper/publication entry
format_paper <- function(p) {
  # Title with optional link (prefer DOI link if available)
  link_to_use <- ifelse(!is.na(p$link) && p$link != "", p$link, 
                       ifelse(!is.na(p$DOI) && p$DOI != "", paste0("https://doi.org/", p$DOI), ""))
  
  if (link_to_use != "") {
    title_line <- paste0("### [", p$title, "](", link_to_use, ")")
  } else {
    title_line <- paste0("### ", p$title)
  }
  
  # Journal info (already formatted in CSV)
  journal_line <- ifelse(is.na(p$journal) || p$journal == "", "", p$journal)
  
  # Year
  year_line <- ifelse(is.na(p$year) || p$year == "", "", as.character(p$year))
  
  # Authors - bold Timothy Fraser
  authors_line <- ifelse(is.na(p$authors) || p$authors == "", "", p$authors)
  if (authors_line != "") {
    # Bold Timothy Fraser if present
    authors_line <- gsub("Timothy Fraser", "**Timothy Fraser**", authors_line, fixed = TRUE)
  }
  
  # Combine
  paste0(
    title_line, "\n\n",
    journal_line, "\n\n",
    "N/A\n\n",
    year_line, "\n\n",
    authors_line
  )
}

#' Format a report entry
format_report <- function(p) {
  # Title with period
  title_line <- paste0("### ", p$title, ".")
  
  # Journal/Series info with link
  journal_line <- ifelse(is.na(p$journal) || p$journal == "", "", p$journal)
  
  # Add link if available and not already in journal line
  if (!is.na(p$link) && p$link != "" && !grepl("http", journal_line) && journal_line != "") {
    # Extract series name if possible
    if (grepl("Natural Hazards Center", journal_line)) {
      series_name <- "Natural Hazards Center Quick Response Grant Report Series"
      rest <- gsub(series_name, "", journal_line)
      journal_line <- paste0("[", series_name, "](", p$link, "), ", trimws(rest))
    } else {
      # Generic link
      journal_line <- paste0("[", journal_line, "](", p$link, ")")
    }
  }
  
  # Year
  year_line <- ifelse(is.na(p$year) || p$year == "", "", as.character(p$year))
  
  # Authors - bold Timothy Fraser
  authors_line <- ifelse(is.na(p$authors) || p$authors == "", "", p$authors)
  if (authors_line != "") {
    authors_line <- gsub("Timothy Fraser", "**Timothy Fraser**", authors_line, fixed = TRUE)
    # Add period if not present
    if (!grepl("\\.$", authors_line)) {
      authors_line <- paste0(authors_line, ".")
    }
  }
  
  # Combine
  paste0(
    title_line, "\n\n",
    journal_line, "\n\n",
    "N/A\n\n",
    year_line, "\n\n",
    authors_line
  )
}

#' Format a chapter entry
format_chapter <- function(p) {
  # Title with optional link and period
  link_to_use <- ifelse(!is.na(p$link) && p$link != "", p$link, "")
  
  if (link_to_use != "") {
    title_line <- paste0("### [", p$title, ".](", link_to_use, ")")
  } else {
    title_line <- paste0("### ", p$title, ".")
  }
  
  # Journal/book info
  journal_line <- ifelse(is.na(p$journal) || p$journal == "", "", p$journal)
  
  # Year
  year_line <- ifelse(is.na(p$year) || p$year == "", "", as.character(p$year))
  
  # Authors - bold Timothy Fraser
  authors_line <- ifelse(is.na(p$authors) || p$authors == "", "", p$authors)
  if (authors_line != "") {
    authors_line <- gsub("Timothy Fraser", "**Timothy Fraser**", authors_line, fixed = TRUE)
  }
  
  # Combine
  paste0(
    title_line, "\n\n",
    journal_line, "\n\n",
    "N/A\n\n",
    year_line, "\n\n",
    authors_line
  )
}

#' Format a software entry
format_software <- function(p) {
  # Extract name from title (first word before space)
  title_clean <- trimws(p$title)
  
  # Extract name (first word, handling special cases like VISUALIZER)
  if (grepl("^VISUALIZER", title_clean, ignore.case = TRUE)) {
    name <- "VISUALIZER"
    title_desc <- trimws(sub("^VISUALIZER\\s+", "", title_clean, ignore.case = TRUE))
  } else {
    # Split by space and take first element as name
    title_parts <- strsplit(title_clean, "\\s+")[[1]]
    name <- ifelse(length(title_parts) > 0, title_parts[1], title_clean)
    
    # Title description is rest of title after name (join remaining parts)
    if (length(title_parts) > 1) {
      title_desc <- paste(title_parts[-1], collapse = " ")
    } else {
      title_desc <- ""
    }
  }
  
  # Remove backticks if present and clean name
  name <- gsub("`", "", name)
  
  # Title line with backticks and optional link
  if (!is.na(p$link) && p$link != "" && title_desc != "") {
    title_line <- paste0("### `", name, "` [", title_desc, "](", p$link, ")")
  } else if (!is.na(p$link) && p$link != "") {
    title_line <- paste0("### `", name, "` [", p$title, "](", p$link, ")")
  } else if (title_desc != "") {
    title_line <- paste0("### `", name, "` ", title_desc)
  } else {
    title_line <- paste0("### `", name, "`")
  }
  
  # Check for description field in CSV (optional)
  description <- ""
  if ("description" %in% names(p)) {
    description <- ifelse(is.na(p$description) || p$description == "", "", trimws(p$description))
  }
  
  # Year
  year_line <- ifelse(is.na(p$year) || p$year == "", "", as.character(p$year))
  
  # Combine with proper line breaks
  # Format: title, blank line, description (if exists), blank line, N/A, blank line, year, blank line
  result <- paste0(title_line, "\n\n")
  
  if (description != "") {
    result <- paste0(result, description, "\n\n")
  }
  
  result <- paste0(result, "N/A\n\n", year_line, "\n\n")
  
  return(result)
}

#' Format a book entry
format_book <- function(p) {
  # Title
  title_line <- paste0("### ", p$title)
  
  # Year
  year_line <- ifelse(is.na(p$year) || p$year == "", "", as.character(p$year))
  
  # Optional link in description (book section has bullet points, so we'll keep it simple)
  # The actual description would need to be added manually or from another field
  
  # Combine
  paste0(
    title_line, "\n\n",
    "N/A\n\n",
    year_line, "\n\n"
  )
}

#' Generate publication section for CV
#' 
#' @param csv_path Path to publications CSV
#' @param section_type Type of section: "Publications", "Reports", "Chapters", "Software", "Book Project"
#' @param include_header Whether to include the section header (default: TRUE)
#' @return Formatted section text
generate_publication_section <- function(csv_path = "docs/cv/publications.csv",
                                         section_type = "Publications",
                                         include_header = TRUE) {
  
  # Map section names to CSV types
  type_map <- list(
    "Publications" = "paper",
    "Reports" = "report",
    "Chapters" = "chapter",
    "Software" = "software",
    "Book Project" = "book"
  )
  
  csv_type <- type_map[[section_type]]
  if (is.null(csv_type)) {
    csv_type <- "paper"
  }
  
  # Get formatted publications
  formatted <- format_publications(csv_path, type_filter = csv_type)
  
  # Initialize result
  result <- ""
  
  # Add section header if requested
  if (include_header) {
    section_header <- paste0(
      section_type, " {data-icon=",
      switch(section_type,
             "Publications" = "file",
             "Reports" = "file",
             "Chapters" = "file",
             "Software" = "screwdriver-wrench",
             "Book Project" = "book-open",
             "file"
      ),
      "}\n",
      paste(rep("-", 80), collapse = ""), "\n\n"
    )
    
    # Add aside for Software section
    if (section_type == "Software") {
      section_header <- paste0(
        section_header,
        "::: aside\n\n",
        "**", length(formatted), "** packages and dashboards\n\n",
        ":::\n\n\n"
      )
    }
    
    result <- section_header
  }
  
  # Combine all
  paste0(result, paste(formatted, collapse = "\n\n"))
}

#' Generate aside block for CV with publication counts by topic
#' 
#' @param csv_path Path to publications CSV file
#' @param type_filter Type of publication to count (e.g., "paper", "software")
#' @param title Title for the aside (e.g., "Peer-reviewed Papers:", "packages and dashboards")
#' @param topic_groups Optional: Named list of topic groups. Each group is a list of topic field names.
#'   Example: list("Emissions/Environmental Policy" = c("topic_environment", "topic_transportation"),
#'                 "Community Resilience, Evacuation, & Health" = c("topic_disaster", "topic_health", "topic_social_capital", "topic_social_infrastructure"),
#'                 "Political Polarization" = c("topic_polarization"))
#' @param show_total Whether to show total count in brackets after title
#' @param show_by_topic Whether to show breakdown by topic groups
#' @param style Style variant: "default" (simple), "card" (styled with borders), "elegant" (with accent colors)
#' @return Formatted aside block as character string
aside <- function(csv_path = "docs/cv/publications.csv",
                 type_filter = "paper",
                 title = "Peer-reviewed Papers:",
                 topic_groups = NULL,
                 show_total = TRUE,
                 show_by_topic = TRUE,
                 style = "card") {
  
  # Read CSV
  pubs <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # Filter by type
  if (!is.null(type_filter)) {
    pubs <- pubs[pubs$type == type_filter, ]
  }
  
  # Count total
  total_count <- nrow(pubs)
  
  # Build aside content
  aside_content <- "::: aside\n\n"
  
  # Style the container based on style parameter
  if (style == "card") {
    aside_content <- paste0(aside_content, 
      "<div style='background-color: #f8f9fa; padding: 12px; border-radius: 6px; border-left: 4px solid #B31B1B; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>\n")
  } else if (style == "elegant") {
    aside_content <- paste0(aside_content,
      "<div style='background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); padding: 12px; border-radius: 6px; border: 1px solid #dee2e6; box-shadow: 0 2px 6px rgba(0,0,0,0.1);'>\n")
  }
  
  # Add title with total count (styled)
  if (show_total) {
    if (style == "default") {
      aside_content <- paste0(aside_content, "**", title, " [", total_count, "]**\n\n")
    } else {
      aside_content <- paste0(aside_content,
        "<div style='color: #B31B1B; font-weight: bold; font-size: 1.05em; margin-bottom: 10px;'>",
        title, " <span style='color: #2C2C2C; font-weight: normal;'>[", total_count, "]</span>",
        "</div>\n")
    }
  } else {
    if (style == "default") {
      aside_content <- paste0(aside_content, "**", total_count, "** ", title, "\n\n")
    } else {
      aside_content <- paste0(aside_content,
        "<div style='color: #B31B1B; font-weight: bold; font-size: 1.05em; margin-bottom: 10px;'>",
        "<span style='color: #2C2C2C;'>", total_count, "</span> ", title,
        "</div>\n")
    }
  }
  
  # Add topic breakdown if requested
  if (show_by_topic && !is.null(topic_groups) && length(topic_groups) > 0) {
    if (style == "default") {
      aside_content <- paste0(aside_content, "By Topic:\n\n")
    } else {
      aside_content <- paste0(aside_content,
        "<div style='color: #495057; font-weight: 600; margin-top: 12px; margin-bottom: 8px; font-size: 0.95em;'>By Topic:</div>\n")
    }
    
    # Count papers in each topic group
    topic_counts <- numeric(length(topic_groups))
    names(topic_counts) <- names(topic_groups)
    
    for (i in seq_along(topic_groups)) {
      group_name <- names(topic_groups)[i]
      topic_fields <- topic_groups[[i]]
      
      # Count papers that have ANY of the topics in this group set to TRUE
      count <- 0
      for (j in seq_len(nrow(pubs))) {
        p <- pubs[j, ]
        # Check if any topic in this group is TRUE
        has_topic <- FALSE
        for (field in topic_fields) {
          if (field %in% names(p)) {
            # Handle both "True"/"False" strings and TRUE/FALSE logicals
            val <- p[[field]]
            if (is.logical(val)) {
              if (val) has_topic <- TRUE
            } else if (is.character(val)) {
              if (tolower(trimws(val)) %in% c("true", "t", "1")) has_topic <- TRUE
            } else if (!is.na(val) && val != "" && val != 0) {
              has_topic <- TRUE
            }
          }
        }
        if (has_topic) count <- count + 1
      }
      topic_counts[i] <- count
    }
    
    # Add topic breakdown lines (styled)
    if (style == "default") {
      for (i in seq_along(topic_counts)) {
        group_name <- names(topic_counts)[i]
        count <- topic_counts[i]
        aside_content <- paste0(aside_content, "- ", group_name, " <span style='color: #B31B1B;'><b>~", count, "</b></span>\n")
      }
    } else {
      for (i in seq_along(topic_counts)) {
        group_name <- names(topic_counts)[i]
        count <- topic_counts[i]
        aside_content <- paste0(aside_content,
          "<div style='margin-bottom: 6px; padding-left: 8px; border-left: 2px solid #B31B1B;'>",
          "<span style='color: #495057;'>", group_name, "</span> ",
          "<span style='color: #B31B1B;'><b>~", count, "</b></span>",
          "</div>\n")
      }
    }
    
    if (style == "default") {
      aside_content <- paste0(aside_content, "\n")
    }
  }
  
  # Close styled container if used
  if (style != "default") {
    aside_content <- paste0(aside_content, "</div>\n")
  }
  
  aside_content <- paste0(aside_content, ":::\n")
  
  return(aside_content)
}

#' Generate card-style aside block with header and body (pagedown compatible)
#' 
#' @param csv_path Path to publications CSV file
#' @param type_filter Type of publication to count (e.g., "paper", "software")
#' @param title Title for the aside header
#' @param topic_groups Optional: Named list of topic groups
#' @param show_total Whether to show total count
#' @param show_by_topic Whether to show breakdown by topic groups
#' @param style Style variant: "card" (default), "card-accent", "card-minimal", "card-elegant"
#' @return Formatted aside block as character string (uses pagedown ::: aside format)
aside_card <- function(csv_path = "docs/cv/publications.csv",
                      type_filter = "paper",
                      title = "Peer-reviewed Papers",
                      topic_groups = NULL,
                      show_total = TRUE,
                      show_by_topic = TRUE,
                      style = "card") {
  
  # Read CSV
  pubs <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # Filter by type
  if (!is.null(type_filter)) {
    pubs <- pubs[pubs$type == type_filter, ]
  }
  
  # Count total
  total_count <- nrow(pubs)
  
  # Build card content using pagedown aside format with CSS classes
  if (style == "card-accent") {
    card_content <- paste0(
      "::: {.aside-card-accent}\n\n",
      "**", title, "**",
      if (show_total) paste0(" <span class='aside-badge'>", total_count, "</span>") else "",
      "\n\n"
    )
  } else if (style == "card-minimal") {
    card_content <- paste0(
      "::: {.aside-card-minimal}\n\n",
      "**", title, "**",
      if (show_total) paste0(" [", total_count, "]") else "",
      "\n\n"
    )
  } else if (style == "card-elegant") {
    card_content <- paste0(
      "::: {.aside-card-elegant}\n\n",
      "<div class='card-header'>\n",
      "<strong>", title, "</strong>\n",
      if (show_total) paste0("<div class='card-count'>", total_count, "</div>\n") else "",
      "</div>\n\n"
    )
  } else {
    # Default card style
    card_content <- paste0(
      "::: {.aside-card}\n\n",
      "<div class='card-header'>\n",
      "<strong>", title, "</strong>",
      if (show_total) paste0(" <span class='card-badge'>[", total_count, "]</span>") else "",
      "\n</div>\n\n"
    )
  }
  
  # Add topic breakdown if requested
  if (show_by_topic && !is.null(topic_groups) && length(topic_groups) > 0) {
    # Count papers in each topic group
    topic_counts <- numeric(length(topic_groups))
    names(topic_counts) <- names(topic_groups)
    
    for (i in seq_along(topic_groups)) {
      group_name <- names(topic_groups)[i]
      topic_fields <- topic_groups[[i]]
      
      # Count papers that have ANY of the topics in this group set to TRUE
      count <- 0
      for (j in seq_len(nrow(pubs))) {
        p <- pubs[j, ]
        # Check if any topic in this group is TRUE
        has_topic <- FALSE
        for (field in topic_fields) {
          if (field %in% names(p)) {
            val <- p[[field]]
            if (is.logical(val)) {
              if (val) has_topic <- TRUE
            } else if (is.character(val)) {
              if (tolower(trimws(val)) %in% c("true", "t", "1")) has_topic <- TRUE
            } else if (!is.na(val) && val != "" && val != 0) {
              has_topic <- TRUE
            }
          }
        }
        if (has_topic) count <- count + 1
      }
      topic_counts[i] <- count
    }
    
    # Add topic breakdown based on style
    if (style == "card-elegant") {
      card_content <- paste0(card_content, "<div class='card-body'>\n")
      card_content <- paste0(card_content, "<div class='topic-section'>By Topic:</div>\n")
    } else {
      card_content <- paste0(card_content, "By Topic:\n\n")
    }
    
    for (i in seq_along(topic_counts)) {
      group_name <- names(topic_counts)[i]
      count <- topic_counts[i]
      
      if (style == "card-elegant") {
        card_content <- paste0(card_content,
                               "<div class='topic-row'>",
                               "<span class='topic-label'>", group_name, "</span>",
                               " <span class='topic-value'>[~", count, "]</span>",
                               "</div>\n")
      } else {
        card_content <- paste0(card_content, "- ", group_name, " [~", count, "]\n")
      }
    }
    
    if (style == "card-elegant") {
      card_content <- paste0(card_content, "</div>\n\n")
    } else {
      card_content <- paste0(card_content, "\n")
    }
  }
  
  # Close aside
  card_content <- paste0(card_content, ":::\n")
  
  return(card_content)
}

#' Generate boxed aside block with border
#' 
#' @param csv_path Path to publications CSV file
#' @param type_filter Type of publication to count
#' @param title Title for the aside
#' @param topic_groups Optional: Named list of topic groups
#' @param show_total Whether to show total count
#' @param show_by_topic Whether to show breakdown by topic groups
#' @return Formatted aside block as character string
aside_box <- function(csv_path = "docs/cv/publications.csv",
                     type_filter = "paper",
                     title = "Peer-reviewed Papers",
                     topic_groups = NULL,
                     show_total = TRUE,
                     show_by_topic = TRUE) {
  
  # Read CSV
  pubs <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # Filter by type
  if (!is.null(type_filter)) {
    pubs <- pubs[pubs$type == type_filter, ]
  }
  
  # Count total
  total_count <- nrow(pubs)
  
  # Build box content using pagedown aside format
  box_content <- paste0(
    "::: {.aside-box}\n\n",
    "<div class='box-header'>\n",
    "<strong>", title, "</strong>",
    if (show_total) paste0(" <span class='box-count'>", total_count, "</span>") else "",
    "\n</div>\n\n",
    "<div class='box-body'>\n"
  )
  
  # Add topic breakdown if requested
  if (show_by_topic && !is.null(topic_groups) && length(topic_groups) > 0) {
    # Count papers in each topic group
    topic_counts <- numeric(length(topic_groups))
    names(topic_counts) <- names(topic_groups)
    
    for (i in seq_along(topic_groups)) {
      group_name <- names(topic_groups)[i]
      topic_fields <- topic_groups[[i]]
      
      count <- 0
      for (j in seq_len(nrow(pubs))) {
        p <- pubs[j, ]
        has_topic <- FALSE
        for (field in topic_fields) {
          if (field %in% names(p)) {
            val <- p[[field]]
            if (is.logical(val)) {
              if (val) has_topic <- TRUE
            } else if (is.character(val)) {
              if (tolower(trimws(val)) %in% c("true", "t", "1")) has_topic <- TRUE
            } else if (!is.na(val) && val != "" && val != 0) {
              has_topic <- TRUE
            }
          }
        }
        if (has_topic) count <- count + 1
      }
      topic_counts[i] <- count
    }
    
    box_content <- paste0(box_content, "<div class='box-topics'>By Topic:</div>\n")
    
    for (i in seq_along(topic_counts)) {
      group_name <- names(topic_counts)[i]
      count <- topic_counts[i]
      box_content <- paste0(box_content, 
                            "<div class='box-topic-item'>",
                            "<span class='box-topic-name'>", group_name, "</span>",
                            " <span class='box-topic-count'>[~", count, "]</span>",
                            "</div>\n")
    }
    
    box_content <- paste0(box_content, "</div>\n\n")
  } else {
    box_content <- paste0(box_content, "</div>\n\n")
  }
  
  box_content <- paste0(box_content, ":::\n")
  
  return(box_content)
}

#' Generate modern aside block with icon and stats
#' 
#' @param csv_path Path to publications CSV file
#' @param type_filter Type of publication to count
#' @param title Title for the aside
#' @param icon Optional: Font Awesome icon class (e.g., "fa-file", "fa-code")
#' @param topic_groups Optional: Named list of topic groups
#' @param show_total Whether to show total count
#' @param show_by_topic Whether to show breakdown by topic groups
#' @return Formatted aside block as character string
aside_modern <- function(csv_path = "docs/cv/publications.csv",
                        type_filter = "paper",
                        title = "Peer-reviewed Papers",
                        icon = "fa-file",
                        topic_groups = NULL,
                        show_total = TRUE,
                        show_by_topic = TRUE) {
  
  # Read CSV
  pubs <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # Filter by type
  if (!is.null(type_filter)) {
    pubs <- pubs[pubs$type == type_filter, ]
  }
  
  # Count total
  total_count <- nrow(pubs)
  
  # Build modern aside content using pagedown format
  modern_content <- paste0(
    "::: {.aside-modern}\n\n",
    "<div class='modern-header'>\n",
    if (!is.null(icon)) paste0("<i class='fa ", icon, "'></i> ") else "",
    "<span class='modern-title'>", title, "</span>\n",
    "</div>\n\n",
    if (show_total) paste0("<div class='modern-stats'>\n",
                          "<div class='stat-big'>", total_count, "</div>\n",
                          "</div>\n\n") else ""
  )
  
  # Add topic breakdown if requested
  if (show_by_topic && !is.null(topic_groups) && length(topic_groups) > 0) {
    # Count papers in each topic group
    topic_counts <- numeric(length(topic_groups))
    names(topic_counts) <- names(topic_groups)
    
    for (i in seq_along(topic_groups)) {
      group_name <- names(topic_groups)[i]
      topic_fields <- topic_groups[[i]]
      
      count <- 0
      for (j in seq_len(nrow(pubs))) {
        p <- pubs[j, ]
        has_topic <- FALSE
        for (field in topic_fields) {
          if (field %in% names(p)) {
            val <- p[[field]]
            if (is.logical(val)) {
              if (val) has_topic <- TRUE
            } else if (is.character(val)) {
              if (tolower(trimws(val)) %in% c("true", "t", "1")) has_topic <- TRUE
            } else if (!is.na(val) && val != "" && val != 0) {
              has_topic <- TRUE
            }
          }
        }
        if (has_topic) count <- count + 1
      }
      topic_counts[i] <- count
    }
    
    modern_content <- paste0(modern_content, "<div class='modern-body'>\n")
    modern_content <- paste0(modern_content, "<div class='modern-topics-label'>By Topic:</div>\n")
    
    for (i in seq_along(topic_counts)) {
      group_name <- names(topic_counts)[i]
      count <- topic_counts[i]
      modern_content <- paste0(modern_content,
                               "<div class='modern-topic-row'>\n",
                               "<span class='modern-topic-label'>", group_name, "</span>\n",
                               "<span class='modern-topic-badge'>", count, "</span>\n",
                               "</div>\n")
    }
    
    modern_content <- paste0(modern_content, "</div>\n\n")
  }
  
  modern_content <- paste0(modern_content, ":::\n")
  
  return(modern_content)
}

#' Generate aside card from raw markdown content
#' 
#' Allows you to input markdown content directly without needing CSV data.
#' Useful for content that isn't yet formalized into the CSV format.
#' 
#' @param content Raw markdown content to display in the aside
#' @param title Optional: Title/header for the card
#' @param style Style variant: "card" (default), "card-accent", "card-minimal", "card-elegant", "box", "modern"
#'   Note: All styles use standard `::: aside` format for pagedown compatibility
#' @param icon Optional: Font Awesome icon class (for modern style)
#' @return Formatted aside block as character string
aside_content <- function(content,
                         title = NULL,
                         style = "card",
                         icon = NULL) {
  
  # Always use standard pagedown aside format
  aside_block <- "::: aside\n\n"
  
  # Style the container based on style parameter
  if (style == "card") {
    aside_block <- paste0(aside_block,
      "<div style='background-color: #f8f9fa; padding: 12px; border-radius: 6px; border-left: 4px solid #B31B1B; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>\n")
  } else if (style == "card-elegant") {
    aside_block <- paste0(aside_block,
      "<div style='background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); padding: 12px; border-radius: 6px; border: 1px solid #dee2e6; box-shadow: 0 2px 6px rgba(0,0,0,0.1);'>\n")
  } else if (style == "card-minimal") {
    aside_block <- paste0(aside_block,
      "<div style='padding: 8px;'>\n")
  } else if (style == "box") {
    aside_block <- paste0(aside_block,
      "<div style='background-color: white; padding: 12px; border: 2px solid #B31B1B; border-radius: 6px; box-shadow: 0 3px 6px rgba(0,0,0,0.15);'>\n")
  }
  
  # Add title if provided (formatting varies by style)
  if (!is.null(title)) {
    if (style == "card" || style == "card-elegant" || style == "box") {
      aside_block <- paste0(aside_block,
        "<div style='color: #B31B1B; font-weight: bold; font-size: 1.05em; margin-bottom: 10px;'>",
        title, "</div>\n")
    } else if (style == "card-minimal") {
      aside_block <- paste0(aside_block, "**", title, "**\n\n")
    } else {
      aside_block <- paste0(aside_block, "**", title, "**\n\n")
    }
  }
  
  # Add the content
  aside_block <- paste0(aside_block, content, "\n")
  
  # Close styled container if used
  if (style %in% c("card", "card-elegant", "card-minimal", "box")) {
    aside_block <- paste0(aside_block, "</div>\n")
  }
  
  aside_block <- paste0(aside_block, "\n")
  
  # Close aside
  aside_block <- paste0(aside_block, ":::\n")
  
  return(aside_block)
}

#' Helper function to generate styled header HTML for aside content
#' 
#' Provides pre-styled HTML wrappers for headers in custom aside content.
#' 
#' @param text Header text
#' @param style Style variant: "accent" (red with border), "bold" (simple bold), "elegant" (gradient), "minimal" (subtle)
#' @return HTML string for styled header
aside_header <- function(text, style = "accent") {
  if (style == "accent") {
    return(paste0(
      "<div style='color: #B31B1B; font-weight: bold; font-size: 1.05em; margin-bottom: 10px; padding-bottom: 6px; border-bottom: 2px solid #B31B1B;'>",
      text, "</div>"
    ))
  } else if (style == "bold") {
    return(paste0(
      "<div style='color: #B31B1B; font-weight: bold; font-size: 1.05em; margin-bottom: 10px;'>",
      text, "</div>"
    ))
  } else if (style == "elegant") {
    return(paste0(
      "<div style='background: linear-gradient(90deg, #B31B1B 0%, transparent 100%); color: #B31B1B; font-weight: bold; font-size: 1.05em; padding: 6px 0 6px 8px; margin-bottom: 10px; border-left: 3px solid #B31B1B;'>",
      text, "</div>"
    ))
  } else if (style == "minimal") {
    return(paste0(
      "<div style='color: #495057; font-weight: 600; font-size: 1em; margin-bottom: 8px;'>",
      text, "</div>"
    ))
  } else {
    return(paste0("<div style='font-weight: bold; margin-bottom: 10px;'>", text, "</div>"))
  }
}

#' Generate aside card with image, title, and caption
#' 
#' Creates a styled card in an aside block with an image, optional title, and caption.
#' 
#' @param image_path Path to the image file (relative to CV directory)
#' @param title Optional: Title/header for the image card
#' @param caption Optional: Caption text below the image
#' @param image_alt Optional: Alt text for the image
#' @param style Style variant: "card" (default), "elegant", "minimal"
#' @param image_width Optional: Width of the image (default: "100%")
#' @param image_height Optional: Height of the image (default: "auto")
#' @return Formatted aside block as character string
aside_image_card <- function(image_path,
                            title = NULL,
                            caption = NULL,
                            image_alt = NULL,
                            style = "card",
                            image_width = "100%",
                            image_height = "auto") {
  
  # Default alt text if not provided
  if (is.null(image_alt)) {
    image_alt <- ifelse(!is.null(title), title, "Image")
  }
  
  # Build aside block
  aside_block <- "::: aside\n\n"
  
  # Style the container
  if (style == "card") {
    aside_block <- paste0(aside_block,
      "<div style='background-color: #f8f9fa; padding: 12px; border-radius: 6px; border-left: 4px solid #B31B1B; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>\n")
  } else if (style == "elegant") {
    aside_block <- paste0(aside_block,
      "<div style='background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); padding: 12px; border-radius: 6px; border: 1px solid #dee2e6; box-shadow: 0 2px 6px rgba(0,0,0,0.1);'>\n")
  } else if (style == "minimal") {
    aside_block <- paste0(aside_block,
      "<div style='padding: 8px;'>\n")
  }
  
  # Add title if provided
  if (!is.null(title)) {
    if (style == "minimal") {
      aside_block <- paste0(aside_block, "**", title, "**\n\n")
    } else {
      aside_block <- paste0(aside_block,
        "<div style='color: #B31B1B; font-weight: bold; font-size: 1.05em; margin-bottom: 10px; text-align: center;'>",
        title, "</div>\n")
    }
  }
  
  # Add image with styling
  aside_block <- paste0(aside_block,
    "<div style='margin-bottom: 10px; text-align: center;'>\n",
    "<img src='", image_path, "' ",
    "alt='", image_alt, "' ",
    "style='width: ", image_width, "; height: ", image_height, "; ",
    "border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.15); ",
    "object-fit: cover; display: block; margin: 0 auto;' />\n",
    "</div>\n")
  
  # Add caption if provided
  if (!is.null(caption)) {
    aside_block <- paste0(aside_block,
      "<div style='color: #6c757d; font-size: 0.9em; text-align: center; font-style: italic; margin-top: 8px; padding-top: 8px; border-top: 1px dashed #dee2e6;'>",
      caption, "</div>\n")
  }
  
  # Close container
  aside_block <- paste0(aside_block, "</div>\n\n")
  
  # Close aside
  aside_block <- paste0(aside_block, ":::\n")
  
  return(aside_block)
}

#' Format Press entries from CSV for CV
#' 
#' Reads press entries from CSV and formats them according to CV style
#' 
#' @param csv_path Path to press CSV file
#' @param type_filter Optional: filter by type (e.g., "quoted_in", "cited_in", "interviewed", "reviewed_in", "op_ed")
#' @param year_filter Optional: filter by year
#' @return Character vector of formatted press entries
format_press <- function(csv_path = "docs/cv/press.csv",
                        type_filter = NULL,
                        year_filter = NULL) {
  
  # Read CSV
  press <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # Apply filters
  if (!is.null(type_filter)) {
    press <- press[press$type == type_filter, ]
  }
  if (!is.null(year_filter)) {
    press <- press[press$year == year_filter, ]
  }
  
  # Sort by year (descending), then by title
  press <- press[order(-as.numeric(press$year), press$title), ]
  
  # Format each press entry
  formatted <- character(nrow(press))
  
  for (i in seq_len(nrow(press))) {
    p <- press[i, ]
    formatted[i] <- format_press_entry(p)
  }
  
  return(formatted)
}

#' Format a single press entry
format_press_entry <- function(p) {
  # Map type to display format
  type_map <- list(
    "quoted_in" = "Quoted in",
    "cited_in" = "Cited in",
    "interviewed" = "Interviewed on",
    "reviewed_in" = "Reviewed in",
    "op_ed" = "Op-Ed in"
  )
  
  type_display <- type_map[[p$type]]
  if (is.null(type_display)) {
    type_display <- "Mentioned in"
  }
  
  # Header: ### Type in *Outlet*
  header <- paste0("### ", type_display, " *", p$outlet, "*")
  
  # Build the main line: Author. [Title](link). Outlet (Media Type), Date.
  main_line <- ""
  
  # Add author if available
  if (!is.na(p$press_author) && p$press_author != "") {
    main_line <- paste0(p$press_author, ". ")
  }
  
  # Add title with link
  if (!is.na(p$link) && p$link != "") {
    main_line <- paste0(main_line, "[", p$title, "](", p$link, ")")
  } else {
    main_line <- paste0(main_line, p$title)
  }
  
  # Add period after title
  if (!grepl("\\.$", main_line)) {
    main_line <- paste0(main_line, ".")
  }
  
  # Add outlet and media type
  media_type_display <- ""
  if (!is.na(p$media_type) && p$media_type != "") {
    # Capitalize first letter and format
    media_type_cap <- paste0(toupper(substring(p$media_type, 1, 1)), 
                             substring(p$media_type, 2))
    # Handle special cases
    if (p$media_type == "online_news") {
      media_type_display <- "Online News"
    } else if (p$media_type == "tv") {
      media_type_display <- "TV"
    } else {
      media_type_display <- media_type_cap
    }
    
    # Add in parentheses
    main_line <- paste0(main_line, " ", p$outlet, " (", media_type_display, ")")
  } else {
    main_line <- paste0(main_line, " ", p$outlet)
  }
  
  # Add date if available
  if (!is.na(p$date) && p$date != "") {
    main_line <- paste0(main_line, ", ", p$date, ".")
  } else {
    # Add period if not present
    if (!grepl("\\.$", main_line)) {
      main_line <- paste0(main_line, ".")
    }
  }
  
  # Year
  year_line <- ifelse(is.na(p$year) || p$year == "", "", as.character(p$year))
  
  # Combine
  paste0(
    header, "\n\n",
    main_line, "\n\n",
    "N/A\n\n",
    year_line, "\n\n"
  )
}

#' Generate press section for CV
#' 
#' @param csv_path Path to press CSV
#' @param include_header Whether to include the section header (default: TRUE)
#' @return Formatted section text
generate_press_section <- function(csv_path = "docs/cv/press.csv",
                                   include_header = TRUE) {
  
  # Get formatted press entries
  formatted <- format_press(csv_path)
  
  # Initialize result
  result <- ""
  
  # Add section header if requested
  if (include_header) {
    # Count total press entries
    press <- read.csv(csv_path, stringsAsFactors = FALSE)
    total_count <- nrow(press)
    
    section_header <- paste0(
      "Press & Public Scholarship (", total_count, ") {data-icon=newspaper}\n",
      paste(rep("-", 80), collapse = ""), "\n\n"
    )
    
    result <- section_header
  }
  
  # Combine all
  paste0(result, paste(formatted, collapse = "\n\n"))
}

#' Generate aside block for CV with press counts by type
#' 
#' @param csv_path Path to press CSV file
#' @param title Title for the aside (e.g., "Press & Public Scholarship:")
#' @param style Style variant: "default" (simple), "card" (styled with borders), "elegant" (with accent colors)
#' @return Formatted aside block as character string
aside_press <- function(csv_path = "docs/cv/press.csv",
                       title = "Press & Public Scholarship:",
                       style = "card") {
  
  # Read CSV
  press <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # Count total
  total_count <- nrow(press)
  
  # Count by type
  type_counts <- table(press$type)
  
  # Map type names to display names
  type_display_map <- list(
    "quoted_in" = "Quoted in",
    "cited_in" = "Cited in",
    "interviewed" = "Interviewed on",
    "reviewed_in" = "Reviewed in",
    "op_ed" = "Op-Ed in"
  )
  
  # Build aside content
  aside_content <- "::: aside\n\n"
  
  # Style the container based on style parameter
  if (style == "card") {
    aside_content <- paste0(aside_content, 
      "<div style='background-color: #f8f9fa; padding: 12px; border-radius: 6px; border-left: 4px solid #B31B1B; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>\n")
  } else if (style == "elegant") {
    aside_content <- paste0(aside_content,
      "<div style='background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); padding: 12px; border-radius: 6px; border: 1px solid #dee2e6; box-shadow: 0 2px 6px rgba(0,0,0,0.1);'>\n")
  }
  
  # Add title with total count (styled)
  if (style == "default") {
    aside_content <- paste0(aside_content, "**", title, " [", total_count, "]**\n\n")
  } else {
    aside_content <- paste0(aside_content,
      "<div style='color: #B31B1B; font-weight: bold; font-size: 1.05em; margin-bottom: 10px;'>",
      title, " <span style='color: #2C2C2C; font-weight: normal;'>[", total_count, "]</span>",
      "</div>\n")
  }
  
  # Add breakdown by type
  if (style == "default") {
    aside_content <- paste0(aside_content, "By Type:\n\n")
  } else {
    aside_content <- paste0(aside_content,
      "<div style='color: #495057; font-weight: 600; margin-top: 12px; margin-bottom: 8px; font-size: 0.95em;'>By Type:</div>\n")
  }
  
  # Sort types by count (descending)
  type_counts_sorted <- sort(type_counts, decreasing = TRUE)
  
  # Add type breakdown lines (styled)
  for (i in seq_along(type_counts_sorted)) {
    type_name <- names(type_counts_sorted)[i]
    count <- type_counts_sorted[i]
    
    # Get display name
    display_name <- type_display_map[[type_name]]
    if (is.null(display_name)) {
      display_name <- type_name
    }
    
    if (style == "default") {
      aside_content <- paste0(aside_content, "- ", display_name, " <span style='color: #B31B1B;'><b>", count, "</b></span>\n")
    } else {
      aside_content <- paste0(aside_content,
        "<div style='margin-bottom: 6px; padding-left: 8px; border-left: 2px solid #B31B1B;'>",
        "<span style='color: #495057;'>", display_name, "</span> ",
        "<span style='color: #B31B1B;'><b>", count, "</b></span>",
        "</div>\n")
    }
  }
  
  if (style == "default") {
    aside_content <- paste0(aside_content, "\n")
  }
  
  # Close styled container if used
  if (style != "default") {
    aside_content <- paste0(aside_content, "</div>\n")
  }
  
  aside_content <- paste0(aside_content, ":::\n")
  
  return(aside_content)
}

