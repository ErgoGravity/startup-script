/*
 * Ergo Node API
 * API docs for Ergo Node. Models are shared between all Ergo products
 *
 * OpenAPI spec version: 4.0.12
 * Contact: ergoplatform@protonmail.com
 *
 * NOTE: This class is auto generated by the swagger code generator program.
 * https://github.com/swagger-api/swagger-codegen.git
 * Do not edit the class manually.
 */

package org.ergoplatform.restapi.client;

import java.util.Objects;
import java.util.Arrays;
import com.google.gson.TypeAdapter;
import com.google.gson.annotations.JsonAdapter;
import com.google.gson.annotations.SerializedName;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import io.swagger.v3.oas.annotations.media.Schema;
import java.io.IOException;
import org.ergoplatform.restapi.client.SigmaBoolean;
/**
 * SigmaBooleanAndPredicate
 */


public class SigmaBooleanAndPredicate extends SigmaBoolean {
  @SerializedName("args")
  private java.util.List<SigmaBoolean> args = new java.util.ArrayList<SigmaBoolean>();

  public SigmaBooleanAndPredicate args(java.util.List<SigmaBoolean> args) {
    this.args = args;
    return this;
  }

  public SigmaBooleanAndPredicate addArgsItem(SigmaBoolean argsItem) {
    this.args.add(argsItem);
    return this;
  }

   /**
   * Get args
   * @return args
  **/
  @Schema(required = true, description = "")
  public java.util.List<SigmaBoolean> getArgs() {
    return args;
  }

  public void setArgs(java.util.List<SigmaBoolean> args) {
    this.args = args;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SigmaBooleanAndPredicate sigmaBooleanAndPredicate = (SigmaBooleanAndPredicate) o;
    return Objects.equals(this.args, sigmaBooleanAndPredicate.args) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(args, super.hashCode());
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SigmaBooleanAndPredicate {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    args: ").append(toIndentedString(args)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }

}
