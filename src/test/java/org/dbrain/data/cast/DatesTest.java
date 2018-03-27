package org.dbrain.data.cast;

import org.junit.Test;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

import static org.assertj.core.api.Assertions.assertThat;

public class DatesTest {

  @Test
  public void test_local_date_to_date() {
    Date expected = Date.from(LocalDate.of(2000, 01, 01).atStartOfDay(ZoneId.systemDefault()).toInstant());
    Date date = Dates.toDate(LocalDate.of(2000, 01, 01), ZoneId.systemDefault());

    assertThat(date).isEqualTo(expected);
  }
}