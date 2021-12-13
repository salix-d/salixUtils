custom_tidyverse_style <- function(){
  styler::create_style_guide(
    initialize = styler:::default_style_guide_attributes,
    line_break = list(
      set_line_break_around_comma_and_or = styler:::set_line_break_around_comma_and_or,
      set_line_break_after_assignment = styler:::set_line_break_after_assignment,
      set_line_break_before_curly_opening = styler:::set_line_break_before_curly_opening,
      remove_line_break_before_round_closing_after_curly = styler:::remove_line_break_before_round_closing_after_curly,
      remove_line_breaks_in_fun_dec = styler:::remove_line_breaks_in_fun_dec,
      style_line_break_around_curly = partial(
        styler:::style_line_break_around_curly,
        strict = TRUE
      ), set_line_break_around_curly_curly = styler:::set_line_break_around_curly_curly,
      set_line_break_before_closing_call = partial(styler:::set_line_break_before_closing_call,
        except_token_before = "COMMENT"
      ),
      set_line_break_after_opening_if_call_is_multi_line = partial(styler:::set_line_break_after_opening_if_call_is_multi_line,
        except_token_after = "COMMENT", except_text_before = c(
          "ifelse",
          "if_else"
        ), force_text_before = c("switch")
      ), remove_line_break_in_fun_call = purrr::partial(styler:::remove_line_break_in_fun_call,
        strict = TRUE
      ), add_line_break_after_pipe = styler:::add_line_break_after_pipe,
      set_linebreak_after_ggplot2_plus = styler:::set_linebreak_after_ggplot2_plus
    ),
    space = list(
      remove_space_before_closing_paren = styler:::remove_space_before_closing_paren,
      remove_space_before_closing_paren = styler:::remove_space_before_opening_paren,
      remove_space_before_comma = styler:::remove_space_before_comma,
      style_space_around_math_token = purrr::partial(
        styler:::style_space_around_math_token,
        strict = TRUE,
        zero = "'^'",
        one = c("'+'", "'-'", "'*'", "'/'")
      ),
      style_space_around_tilde = purrr::partial(
        styler:::style_space_around_tilde,
        strict = TRUE
      ),
      spacing_around_op = purrr::partial(
        styler:::set_space_around_op,
        strict = TRUE
      ),
      remove_space_after_opening_paren = styler:::remove_space_after_opening_paren,
      remove_space_after_excl = styler:::remove_space_after_excl,
      set_space_after_bang_bang = styler:::set_space_after_bang_bang,
      remove_space_before_dollar = styler:::remove_space_before_dollar,
      remove_space_after_fun_dec = styler:::remove_space_after_fun_dec,
      remove_space_around_colons = styler:::remove_space_around_colons,
      start_comments_with_space = purrr::partial(
        styler:::start_comments_with_space,
        force_one = TRUE
      ),
      remove_space_after_unary_pm_nested = styler:::remove_space_after_unary_pm_nested,
      spacing_before_comments = styler:::set_space_before_comments,
      # set_space_between_levels = styler:::set_space_between_levels,
      remove_space_between_levels = function(pd_flat){
        if(pd_flat$token[1] %in% c("FUNCTION", "IF", "WHILE")){
          index <- pd_flat$token == "')'" & pd_flat$token_after == "'{'"
          pd_flat$spaces[index] <- 0L
        } else if(pd_flat$token[1] == "FOR"){
          index <- pd_flat$token == "forcond" & pd_flat$token_after == "'{'"
          pd_flat$spaces[index] <- 0L
        }
        pd_flat
      },
      set_space_between_eq_sub_and_comma = styler:::set_space_between_eq_sub_and_comma,
      set_space_in_curly_curly = styler:::set_space_in_curly_curly
    ),
    token = list(
      fix_quotes = styler:::fix_quotes,
      force_assignment_op = styler:::force_assignment_op,
      resolve_semicolon = styler:::resolve_semicolon,
      add_brackets_in_pipe = styler:::add_brackets_in_pipe,
      wrap_if_else_while_for_fun_multi_line_in_curly = styler:::wrap_if_else_while_for_fun_multi_line_in_curly
    ),
    indention = list(
      indent_braces = partial(styler:::indent_braces, indent_by = 2),
      unindent_fun_dec = styler:::unindent_fun_dec,
      indent_op = partial(styler:::indent_op,
        indent_by = 2
      ), indent_eq_sub = partial(styler:::indent_eq_sub,
        indent_by = 2
      ), indent_without_paren = partial(styler:::indent_without_paren,
        indent_by = 2
      ), update_indention_ref_fun_dec = styler:::update_indention_ref_fun_dec
    ),
    use_raw_indention = FALSE,
    reindention = styler:::tidyverse_reindention(),
    style_guide_name = "custom-tidyverse-style",
    style_guide_version = "v1",
    transformers_drop = styler:::specify_transformers_drop()
  )
}
# styler::style_dir(recursive = FALSE, style = custom_tidyverse_style)
