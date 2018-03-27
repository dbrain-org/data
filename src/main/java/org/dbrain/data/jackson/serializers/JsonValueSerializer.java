/*
 * Copyright [2015] [Eric Poitras]
 *
 *     Licensed under the Apache License, Version 2.0 (the "License");
 *     you may not use this file except in compliance with the License.
 *     You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dbrain.data.jackson.serializers;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.dbrain.data.tree.Node;
import org.dbrain.data.tree.NodeList;
import org.dbrain.data.tree.NodeMap;

import java.io.IOException;
import java.util.Map;

/**
 * Created by epoitras on 08/01/15.
 */
public class JsonValueSerializer extends JsonSerializer<Node> {

    private void writeMap(NodeMap value, JsonGenerator w ) throws IOException {
        w.writeStartObject();
        try {
            for ( Map.Entry<String, Node> e : value.entrySet() ) {
                w.writeFieldName( e.getKey() );
                writeValue( e.getValue(), w );
            }
        } finally {
            w.writeEndObject();
        }
    }

    private void writeList(NodeList value, JsonGenerator w ) throws IOException {
        w.writeStartArray();
        try {
            for ( Node e : value ) {
                writeValue( e, w );
            }
        } finally {
            w.writeEndObject();
        }
    }

    /**
     * Write a value to a generator.
     */
    public void writeValue(Node node, JsonGenerator w ) throws IOException {
        if ( node == null || node.isNull() ) {
            w.writeNull();
        } else if ( node instanceof NodeMap) {
            writeMap( (NodeMap) node, w );
        } else if ( node instanceof NodeList) {
            writeList( (NodeList) node, w );
        } else {
            w.writeObject( node.getObject() );
        }
    }


    @Override
    public void serialize( Node node,
                           JsonGenerator jgen,
                           SerializerProvider provider ) throws IOException, JsonProcessingException {
        writeValue(node, jgen );
    }
}
