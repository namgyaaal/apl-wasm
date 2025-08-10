/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright © 2008-2023  Dr. Jürgen Sauermann

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/** @file
*/

#include <errno.h>
#include <string.h>
#include <sys/fcntl.h>   // for O_RDONLY
#include <sys/stat.h>

#include "Avec.hh"
#include "Quad_FIO.hh"
#include "Quad_XML.hh"
#include "Value.hh"
#include "Workspace.hh"

Quad_XML  Quad_XML::fun;

//----------------------------------------------------------------------------
XML_node::XML_node(XML_node * anchor, const UCS_string & string_B,
                   ShapeItem spos, ShapeItem slen)
  : src(string_B),
    src_pos(spos),
    src_len(slen),
    next(this),
    prev(this),
    err_loc(0),
    level(-1),     // set in translate()
    position(-1)   // set in collect()
{
   if (!anchor)   return;

   // insert this node at the end of the list that starts at anchor
   //
   prev = anchor->prev;   // the (old) end of the list
   anchor->prev = this;   // make this node the new end of the list

   next = anchor;         // this is the end, so this->next is the anchor
   prev->next = this;     // and this is the next of the old end

   // figure the node type
   //
  node_type = NT_error;
  if (src[src_pos] != '<')
     {
       node_type = NT_text;
       return;
     }

  if (src_len < 3)
     {
       err_loc = LOC;
       MORE_ERROR() << "⎕XML: Tag too short: "
                    << UCS_string(src, src_pos, src_len);
       return;
     }

   // <...
   //
   if (src[src_pos + src_len - 1] != '>')
      {
        err_loc = LOC;
        MORE_ERROR() << "⎕XML: No tag end: "
                     << UCS_string(src, src_pos, src_len);
        return;
      }

   // <...>
   //
const Unicode ucs1 = src[src_pos + 1];          // the character after <
   if (ucs1 == '!')   // XML declaration or comment <! ... >
      {
        if (src_len < 7)   // at least <!---->
           {
             err_loc = LOC;
             MORE_ERROR() << "⎕XML: comment not properly terminated: "
                          << UCS_string(src, src_pos, src_len);
             return;
           }

        if (src[src_pos + 2] =='D' &&
            src[src_pos + 3] =='O' &&
            src[src_pos + 4] =='C' &&
            src[src_pos + 5] =='T' &&
            src[src_pos + 6] =='Y' &&
            src[src_pos + 7] =='P' &&
            src[src_pos + 8] =='E')   // <!DOCTYPE ...
           {
             node_type = NT_doctype;
             return;
           }

        if (src[src_pos + 2] !='-' ||
            src[src_pos + 3] !='-')        // not <!--
           {
             err_loc = LOC;
             MORE_ERROR() << "⎕XML: bad comment start (expecting <!--) : "
                          << UCS_string(src, src_pos, src_len);
             return;
           }

        else if (src[src_pos + src_len - 2] != '-' ||
                 src[src_pos + src_len - 3] != '-')
           {
             err_loc = LOC;
             MORE_ERROR() << "⎕XML: bad comment end (expecting --> ): "
                          << UCS_string(src, src_pos, src_len);
             return;
           }
        node_type = NT_comment;
      }
   else if (ucs1 == '?')   // <? ... >
      {
        if (src_len < 7)   // at least <?xml?>
           {
             err_loc = LOC;
             MORE_ERROR() << "⎕XML: processing instruction not terminated: "
                          << UCS_string(src, src_pos, src_len);
             return;
           }
        else if (src[src_pos + 2] !='x' ||
                 src[src_pos + 3] !='m' ||
                 src[src_pos + 4] !='l')   // not <?xml
           {
             err_loc = LOC;
             MORE_ERROR() << "⎕XML: bad declaration start (expecting <?xml): "
                          << UCS_string(src, src_pos, src_len);
             return;
           }

        else if (src[src_pos + src_len - 2] != '?')
           {
             err_loc = LOC;
             MORE_ERROR() << "⎕XML: bad declaration end (expecting ?>): "
                          << UCS_string(src, src_pos, src_len);
             return;
           }
        node_type = NT_declaration;
      }
   else if (ucs1 == '/')   // </ ... >
      {
        if (!is_first_name_char(src[src_pos + 2]))
           {
             err_loc = LOC;
             MORE_ERROR() << "⎕XML: bad tag name in end tag : "
                          << UCS_string(src, src_pos, src_len);
             return;
           }
        node_type = NT_end_tag;
      }
   else if (src[src_pos + src_len - 2] == '/')   // leaf tag
      {
        if (!is_first_name_char(ucs1))
           {
             err_loc = LOC;
             MORE_ERROR() << "⎕XML: bad tag name in empty (-leaf) tag: "
                          << UCS_string(src, src_pos, src_len);
             return;
           }
        node_type = NT_leaf_tag;
      }
   else                              // start tag
      {
        if (!is_first_name_char(ucs1))
           {
             err_loc = LOC;
             MORE_ERROR() << "⎕XML: bad tag name in start tag: "
                          << UCS_string(src, src_pos, src_len);
             return;
           }
        node_type = NT_start_tag;
      }

   Assert(node_type != NT_error);
}
//----------------------------------------------------------------------------
XML_node::~XML_node()
{
}
//----------------------------------------------------------------------------
void
XML_node::print(ostream & out) const
{
   if (err_loc)   CERR << "    errloc: " << err_loc << endl;

const UCS_string item(src, src_pos, src_len);
   out << get_node_type_name() << "[" << level << "] '" << item << "'" << endl;
}
//----------------------------------------------------------------------------
void
XML_node::print_all(ostream & out, const XML_node & anchor)
{
   for (const XML_node * x = anchor.get_next(); x != &anchor; x = x->get_next())
       {
          x->print(CERR);
       }
}
//----------------------------------------------------------------------------
const char *
XML_node::get_node_type_name() const
{
   switch(node_type)
      {
        case NT_error:       return "NT_err ";
        case NT_text:        return "NT_text";
        case NT_leaf_tag:    return "NT_leaf";
        case NT_start_tag:   return "NT_stag";
        case NT_end_tag:     return "NT_etag";
        case NT_comment:     return "NT_comm";
        case NT_declaration: return "NT_decl";
        case NT_doctype:     return "NT_doct";
      }

   return "--???";
}
//============================================================================
Token
Quad_XML::eval_B(Value_P B) const
{
   if (B->is_structured())   // associative B array to XML string
      {
        Value_P Z = APL_to_XML(*B);
        Z->check_value(LOC);
        return Token(TOK_APL_VALUE1, Z);
      }

   if (B->get_rank() != 1)   RANK_ERROR;

Value_P Z = XML_to_APL(*B);
   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
Value_P
Quad_XML::APL_to_XML(const Value & B)
{
vector<const UCS_string *> entities;
   add_sorted_entities(entities, B);

ShapeItem len_Z = 0;
   loop(e, entities.size())   len_Z += entities[e]->size();

Value_P Z(len_Z, LOC);
   loop(e, entities.size())
       {
         const UCS_string & entity = *entities[e];
         const ShapeItem len = entity.size();
         loop(l, len)   Z->next_ravel_Char(entity[l]);
         delete entities[e];
       }

   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
void
Quad_XML::add_sorted_entities(vector<const UCS_string *> & entities,
                              const Value & B)
{
   Assert(B.is_structured());

std::vector<ShapeItem> member_indices;
   B.sorted_members(member_indices, /* filter */ 0);

UCS_string start_tag_name;   // for constructing the corresponding end tag
UCS_string start_tag;        // the start tag
bool start_tag_open = false;
bool tag_open = false;

   loop(m, member_indices.size())
       {
         ShapeItem member_pos;     // the position in the XML file
         Unicode category;
         UCS_string name;
         const Value & member_name = *B.get_cravel(2*member_indices[m])
                                       .get_pointer_value();

         split_name(&category, &member_pos, &name, member_name);

         const Value & member_data = *B.get_cravel(2*member_indices[m] + 1)
                                      .get_pointer_value();

         // one tag; the member_indices items occur in the following
         // order (also below):
         //
         //  ⍙1               start tag name
         //  ⍙NNNattr         attribute N
         //  ∆NNN and _NNN    declarations, unstructured text and subnodes
         //
         if (category == UNI_DELTA_UNDERBAR)
            {
              if (name.size() == 0)   // start of next start tag
                 {
                   if (start_tag_open)   // previous tag still open
                      {
                        start_tag.append_UTF8("/>");
                        entities.push_back(new UCS_string(start_tag));
                        start_tag_open = false;
                      }
                   else if (tag_open)
                      {
                        DOMAIN_ERROR;
                      }

                   start_tag_name = UCS_string(member_data);
                   start_tag.clear();
                   start_tag += UNI_LESS;   // <
                   start_tag.append(start_tag_name);
                   start_tag_open = true;
                   tag_open = true;
                 }
              else                       // tag attribute
                 {
                   if (!tag_open)
                      {
                        DOMAIN_ERROR;
                      }

                   start_tag += UNI_SPACE;
                   start_tag.append(UCS_string(name));
                   start_tag += UNI_EQUAL;
                   start_tag.append(XML_node::denormalize_attribute_value(
                                              UCS_string(member_data), true));
                 }
            }
         else   // ∆ or _
            {
              if (start_tag_open)   // start tag still open
                 {
                   start_tag.append_UTF8(">");
                   entities.push_back(new UCS_string(start_tag));
                   start_tag_open = false;
                 }

              if (category == UNI_DELTA)
                  {
                    entities.push_back(new UCS_string(member_data));
                  }
               else if (category == UNI_UNDERSCORE)
                  {
                    add_sorted_entities(entities, member_data);
                  }
               else
                  {
                    DOMAIN_ERROR;
                  }
            }
       }

   // all items processed.
   //
   if (tag_open)
      {
        if (start_tag_open)   // <TAG att=val att=val...   : close tag
           {
             start_tag.append_UTF8("/>");
           }
        else                // <TAG> ...  : add end tag
           {
             start_tag.clear();
             start_tag.append_UTF8("</");
             start_tag.append(start_tag_name);
             start_tag += UNI_GREATER;
           }
        entities.push_back(new UCS_string(start_tag));
      }
}
//----------------------------------------------------------------------------
Value_P
Quad_XML::XML_to_APL(const Value & B)
{
const ShapeItem len_B = B.element_count();
   if (len_B == 0)   LENGTH_ERROR;

UCS_string string_B(len_B, UNI_NUL);

ShapeItem dest_B = 0;
   loop(src_B, len_B)
      {
        const Unicode uni = B.get_cravel(src_B).get_char_value();
        if (uni == UNI_CR)   // CR
           {
             // see XML standard "2.11 End-of-Line Handling"
             //
             if (src_B == (len_B - 1))          // CR at the end of the document
                {
                  string_B[dest_B++] = UNI_LF;
                  continue;
                }

             const Unicode uni_1 = B.get_cravel(src_B + 1).get_char_value();
             if (string_B[uni_1] == UNI_LF)   // CR/LF
                {
                  continue;   // discard CR (don't ++dest_B)
                }

             else                             // CR not followed by LF
                {
                  string_B[dest_B++] = UNI_LF;   // replace CR with LF
                  continue;
                }

             // not reached
             //
             FIXME;
           }

        if (XML_node::is_XML_char(uni))   // valid XML character
           {
             string_B[dest_B++] = uni;
           }
         else                             // invalid XML character
           {
             char uni_cc[20];
             SPRINTF(uni_cc, "U+%4.4X", uni);
             MORE_ERROR() << "⎕XML B: Invalid XML character " << uni_cc
                          << " at B[" << (src_B + Workspace::get_IO()) << "]";
             DOMAIN_ERROR;
           }
      }

   Assert(dest_B <= len_B);
   string_B.resize(dest_B);

XML_node anchor(0,  string_B, 0, 0);
XML_node garbage(0, string_B, 0, 0);
ShapeItem text_start = 0;
Value_P Z = EmptyStruct(LOC);
bool error = true;

   // create a (flat) doubly-linked list, starting at anchor, and containing
   // all XML_nodes of B.
   //
   loop(b, len_B)
       {
         const Unicode uni_b = B.get_cravel(b).get_char_value();
         if (uni_b == '<')
            {
              if (text_start != b)   // some text before the '<'
                 {
                   const ShapeItem text_len = b - text_start;
                   new XML_node(&anchor, string_B, text_start, text_len);
                 }

              const ShapeItem taglen = XML_node::get_taglen(string_B, b);
              if (taglen == -1)   goto cleanup;

              new XML_node(&anchor, string_B, b, taglen);
              b += taglen;
              text_start = b;
            }
       }

   if (text_start != len_B)   // trailing unstructured text
      new XML_node(&anchor, string_B, text_start, len_B - text_start);

  error = XML_node::translate(anchor, garbage);
  if (!error)   error = XML_node::collect(anchor, garbage, Z.get());

cleanup:
   while (anchor.get_next() != &anchor)     delete anchor.get_next()->unlink();
   while (garbage.get_next() != &garbage)   delete garbage.get_next()->unlink();

   if (error)   DOMAIN_ERROR;
   return Z;
}
//----------------------------------------------------------------------------
ShapeItem
XML_node::get_taglen(const UCS_string & string_B, ShapeItem offset)
{
   Assert(string_B[offset] == '<');

bool is_doctype = true;
const char * cdoc = "!DOCTYPE";
bool inside_dq  = false;   // inside "..."
bool inside_sq  = false;   // inside '...'

   for (ShapeItem pos = offset + 1; pos < string_B.size();)
       {
         const Unicode uni = string_B[pos++];

         if (*cdoc && uni != *cdoc++)   is_doctype = false;

         if (uni == '\'' && !inside_dq)        inside_sq = !inside_sq;
         else if (uni == '"' && !inside_sq)    inside_dq = !inside_dq;
         else if (uni == '<')   // invalid tag start inside tag attribute
            {
              if (inside_sq || inside_dq)   MORE_ERROR() <<
                 "⎕XML B: Unescaped '<' in attribute value: '"
                 << UCS_string(string_B, offset, 20) << "'...";
              else                          MORE_ERROR() <<
                 "⎕XML B: Unescaped '<' in attribute name: '"
                 << UCS_string(string_B, offset, 20) << "'...";
              return -1;
            }
         else if (uni == '>' && !(inside_sq || inside_dq))   // end of tag
            {
              return pos - offset;
            }

         if (is_doctype && !*cdoc)   // rarely: <!DOCTYPE...
            {
              size_t level = 1;
              for (; pos < string_B.size();)
                  {
                    const Unicode uni = string_B[pos++];
                    if (uni == '<')   ++level;
                    else if (uni == '>')
                      {
                        --level;
                        if (level == 0)
                           {
                             return pos - offset;
                           }
                      }
                  }
            }
       }

const int len = string_B.size() - offset;
const int len1 = len > 50 ? 50 : len;
UCS_string where(string_B, offset, len1);
   if (len < 50)   MORE_ERROR() << "No tag end found in: " << where;
   else            MORE_ERROR() << "No tag end found in: " << where;
   return -1;
}
//----------------------------------------------------------------------------
bool
XML_node::translate(XML_node & anchor, XML_node & garbage)
{
int level = 0;

   // reduce text and leaf nodes...
   //
   for (XML_node * node = anchor.get_next(); node != &anchor;
        node = node->get_next())
       {
         node->level = level;
         switch(node->get_node_type())
            {
              case NT_text:
              case NT_comment:
              case NT_declaration:
              case NT_doctype:
                   if (node->get_src_len())   // not empty
                      {
                        const UCS_string item = node->get_item();
                        node->APL_value = Value_P(item, LOC);
                      }
                   else   // empty string
                      {
                        node = node->get_prev();   // move back
                        garbage.append(node->get_next()->unlink());
                      }
                   break;

              case NT_start_tag:
                   ++level;
                   /* fall through */
              case NT_leaf_tag:
                   if (node->parse_tag())   return true;
                   break;

              case NT_end_tag:
                   node->level = --level;
                   break;

              case NT_error:
                   return true;
            }
       }

   return false;   // OK
}
//----------------------------------------------------------------------------
bool
XML_node::collect(XML_node & anchor, XML_node & garbage, Value * Z)
{
XML_node * root = 0;

vector<XML_node *> stack;   // a stack of start tags
vector<size_t> pos_stack;   // a stack of node positions

   // set the positions (before removing any nodes)
   //
   {
     size_t position = Workspace::get_IO();
     for (XML_node * n = anchor.get_next(); n != &anchor; n = n->get_next())
       {
         n->position = position++;
       }
   }

   for (XML_node * node = anchor.get_next(); node != &anchor;
        node = node->get_next())
       {
         size_t position = node->position;
         switch(node->get_node_type())
            {
              case NT_declaration:
                   if (stack.size())   // only allowed at top-level
                      {
                        MORE_ERROR() <<
                        "⎕XML: XML declaration below top-level: "
                        << node->get_item();
                        return true;
                      }

                   add_member(Z, UNI_DELTA, "declaration", position,
                              node->APL_value.get());
                   node = node->get_prev();   // move back
                   garbage.append(node->get_next()->unlink());
                   break;

              case NT_doctype:
                   if (stack.size())   // only allowed at top-level
                      {
                        MORE_ERROR() <<
                        "⎕XML: !DOCTYPE below top-level: "
                        << node->get_item();
                        return true;
                      }

                   add_member(Z, UNI_DELTA, "doctype", position,
                              node->APL_value.get());
                   node = node->get_prev();   // move back
                   garbage.append(node->get_next()->unlink());
                   break;

              case NT_comment:
                   if (stack.size())   continue;   // only top-level comments

                   add_member(Z, UNI_DELTA, "comment", position,
                              node->APL_value.get());
                   node = node->get_prev();
                   garbage.append(node->get_next()->unlink());
                   break;

              case NT_text:
                   if (stack.size())   continue;   // only top-level texts
                   add_member(Z, UNI_DELTA, "text", position,
                              node->APL_value.get());
                   node = node->get_prev();
                   garbage.append(node->get_next()->unlink());
                   break;

              case NT_start_tag:
                   {
                     if (root == 0)   root = node;   // remember the root
                     else if (stack.size() == 0)   // subsequent root
                        {
                          MORE_ERROR() << "⎕XML: more than one root";
                          return true;
                        }
                     Assert(stack.size() == size_t(node->level));
                     stack.push_back(node);
                     pos_stack.push_back(position);
                     position = Workspace::get_IO();   // ⎕IO is ⍙
                   }
                   break;

              case NT_end_tag:
                   {
                     if (stack.size() == 0)   // no start tag
                        {
                          MORE_ERROR() << "⎕XML: end tag " << node->get_item()
                                       << " without start tag";
                          return true;
                        }
                     XML_node * start = stack.back();
                     stack.pop_back();

                     if (!start->matches(node))
                        {
                          MORE_ERROR() << "⎕XML: end tag " << node->get_item()
                                       << " does not match start tag "
                                       << start->get_item();
                          return true;
                        }

                     position = pos_stack.back() + 1;
                     pos_stack.pop_back();
                     merge_range(*start, *node, garbage);
                     node = start;
                   }
                   break;

              case NT_leaf_tag:
                   if (root == 0)   root = node;   // remember the root
                   break;

              case NT_error: FIXME;
            }
       }

   if (root)
      {
        UTF8_string root_utf(root->get_tagname());
        add_member(Z, UNI_UNDERSCORE, root_utf.c_str(), -1,
                   anchor.get_next()->APL_value.get());
      }
   else
      {
        MORE_ERROR() << "⎕XML B: no XML root element in B";
        return true;
      }

   return false;   // OK
}
//----------------------------------------------------------------------------
void
XML_node::add_member(Value * Z, Unicode first, const char * cp_member_name,
                     int number, Value * member_value)
{
UCS_string member_name(first);
   if (number >= 0)   member_name.append_number(number);

   member_name.append_UTF8(cp_member_name);
   Z->add_member(member_name, member_value);
}
//----------------------------------------------------------------------------
bool
XML_node::merge_range(XML_node & start, XML_node & end, XML_node & garbage)
{
   // start is a start tag and end its corresponding end tag. Remove the
   // the nodes after start from the level of start and make them members
   // of start instead.
   //
   Assert(+start.APL_value);   // start contains at least its tagname member (⍙)
   Assert(start.APL_value->is_structured());

size_t position = Workspace::get_IO();   // re-number sub nodes
   for (;;)
       {
         XML_node & sub = *(start.get_next());
         sub.position = position++;

         garbage.append(sub.unlink());   // remove from start's level

         if (sub.get_node_type() == NT_end_tag)   break;

         Assert(+sub.APL_value);
         switch(sub.get_node_type())
            {
              case NT_text:
                   add_member(start.APL_value.get(), UNI_DELTA, "text",
                              sub.position, sub.APL_value.get());
                   break;

              case NT_comment:
                   add_member(start.APL_value.get(), UNI_DELTA, "comment",
                              sub.position, sub.APL_value.get());
                   break;

              case NT_start_tag:
              case NT_leaf_tag:
                   {
                      UCS_string member_name(UNI_UNDERSCORE);
                      member_name.append(sub.get_tagname());
                   start.APL_value->add_member(member_name,
                                               sub.APL_value.get());
                   }
                   break;

              case NT_declaration:
                   add_member(start.APL_value.get(), UNI_DELTA, "declaration",
                              sub.position, sub.APL_value.get());
                   break;

              case NT_doctype:
                   add_member(start.APL_value.get(), UNI_DELTA, "doctype",
                              sub.position, sub.APL_value.get());
                   break;

              case NT_error:
              case NT_end_tag: FIXME;
            }
       }

   return false;   // OK
}
//----------------------------------------------------------------------------
bool
XML_node::parse_tag()
{
   // return Z which is a structured variable with member ⍙ set to the name
   // in the tag and attname-N to the names of the attributes
   //
   APL_value = EmptyStruct(LOC);
const size_t start = src_pos + 1;   // start of tag_name
size_t pos = start;                 // distance from  start

   // parse the tag name
   {
     while (is_name_char(src[pos]))   ++pos;
     const UCS_string tag_name(src, src_pos + 1, pos - start);
     Value_P  member_value(tag_name, LOC);

     add_member(APL_value.get(), UNI_DELTA_UNDERBAR, "",
                Workspace::get_IO(), member_value.get());
   }

const size_t end = src_pos + src_len;   // end of tag
size_t attribute_count = Workspace::get_IO();   // "⍙" gets ⎕IO
   for (;;)
      {
        // skip leading whitespace
        //
        while (pos < end && src[pos] <= UNI_SPACE)   ++pos;

        if (pos >= end || !is_first_name_char(src[pos]))   break;
        // atribute name
        //
        const size_t attname = pos;
        while (pos < end && is_name_char(src[pos]))   ++pos;
        UCS_string attribute_name(src, attname, pos - attname);

        // skip whitespace before =
        //
        while (pos < end && src[pos] <= UNI_SPACE)   ++pos;

        if (src[pos] != UNI_EQUAL)
           {
             MORE_ERROR() << "No '=' after attribute name '" << attribute_name;
             return true;
           }
        ++pos;   // skip =

        // skip whitespace after =
        //
        while (pos < end && src[pos] <= UNI_SPACE)   ++pos;

        const Unicode start_quote = src[pos++];   // ' or "
        if (start_quote != UNI_SINGLE_QUOTE &&
            start_quote != UNI_DOUBLE_QUOTE)
           {
             MORE_ERROR() << "No ' or \" after attribute= '"
                         << attribute_name << "'";
             return true;
           }

        const size_t attvalue = pos;
        while (pos < end && src[pos] != start_quote)   ++pos;
        UCS_string attribute_value(src, attvalue-1, pos + 2 - attvalue);
        ++pos;   // skip trailing ' or "
        if (normalize_attribute_value(attribute_value))   return true;

        UTF8_string member_name(attribute_name);
        Value_P member_value(attribute_value, LOC);
        add_member(APL_value.get(), UNI_DELTA_UNDERBAR, member_name.c_str(),
                   ++attribute_count, member_value.get());
      }

   // skip whitespace after last attribute
   //
   while (pos < end && src[pos] <= UNI_SPACE)   ++pos;

   return false;   // OK
}
//----------------------------------------------------------------------------
bool
XML_node::normalize_attribute_value(UCS_string & attval)
{
   // XML standard "4.6 Predefined Entities"

ShapeItem dest = 0;

struct _mapping
{
  const char * entity;
  int          string_len;
  Unicode      replacement;
};

const _mapping predefined_entities[] =
{ { "&lt;",   4, UNI_LESS         },
  { "&gt;",   4, UNI_GREATER      },
  { "&amp;",  5, UNI_AMPERSAND    },
  { "&apos;", 6, UNI_SINGLE_QUOTE },
  { "&quot;", 6, UNI_DOUBLE_QUOTE }
};

enum { PREDEFINED_COUNT = sizeof(predefined_entities)
                        / sizeof(*predefined_entities) };

   loop(src, attval.size())
       {
         const Unicode uni = attval[src];
         if (uni < UNI_SPACE)
            {
              attval[dest++] = UNI_SPACE;
              continue;
            }

         if (uni != UNI_AMPERSAND)   // normal char
            {
              attval[dest++] = uni;
              continue;
            }

         // &#xxx
         //
         if ((src + 2) < attval.size() && attval[src + 1] == '#')
            {
              src += 2;   // skip &#
              int number = 0;
              Unicode bad_char = UNI_NUL;
              if (attval[src] == 'x')   // hex value &#xHH
                 {
                   ++src;   // skip 'x'
                   while (src < attval.size() && !bad_char)
                      {
                        const Unicode digit = attval[src++];
                        if (digit == ';')   break;   // end of hex digits
                        const int digval = Avec::digit_value(digit, true);
                        if (digval == -1)   bad_char = digit;
                        else                number = 16*number + digval;
                      }
                 }
              else                      // decimal value &#DDD
                 {
                   while (src < attval.size())
                      {
                        const Unicode digit = attval[src++];
                        if (digit == ';')   break;   // end of decimal digits
                        const int digval = Avec::digit_value(digit, false);
                        if (digval == -1)   bad_char = digit;
                        else                number = 10*number + digval;
                      }
                 }

              if (bad_char)
                 {
                   MORE_ERROR() << "⎕XML: bad character '"
                                << UCS_string(1, bad_char)
                                << "' in attribute value '" << attval << "'";
                   return true;
                 }
              attval[dest++] = Unicode(number);
              continue;
            }

         // &... but not &#... Try predefined entities
         //
         loop(p, PREDEFINED_COUNT)
             {
               const _mapping & pred = predefined_entities[p];
               const int len = pred.string_len;
               if ((src + len) > attval.size())   continue;   // too short

               bool match = true;
               loop(ll, len)
                   {
                     if (pred.entity[ll] != attval[src + ll])   // mismatch
                        {
                          match = false;
                          break;   // loop(ll)
                        }
                   }

               if (match)
                  {
                    attval[dest++] = pred.replacement;
                    src += len - 1;   // except uni
                    break;   // loop(p)
                  }
             }
       }

   attval.resize(dest);
   return false;   // OK
}
//----------------------------------------------------------------------------
UCS_string
XML_node::denormalize_attribute_value(const UCS_string & attval, bool quoted)
{
UCS_string ret;
   ret.reserve(attval.size() + 100);

   loop(a, attval.size())
      {
        const Unicode uni = attval[a];
        if (quoted)
           {
             if (a == 0 || a == (attval.size() - 1))
                {
                  ret += uni;
                  continue;
                }
           }

        switch(uni)
            {
              case 0x00 ... 0x1F:
                   {
                     char cc[20];
                     SPRINTF(cc, "&#x%X;", uni);
                     ret.append_UTF8(cc);
                   }
                   continue;

              case UNI_LESS:         ret.append_UTF8("&lt;");     continue;
              case UNI_GREATER:      ret.append_UTF8("&gt;");     continue;
              case UNI_AMPERSAND:    ret.append_UTF8("&amp;");    continue;
              case UNI_SINGLE_QUOTE: ret.append_UTF8("&apos;");   continue;
              case UNI_DOUBLE_QUOTE: ret.append_UTF8("&quot;");   continue;
              default: ret += uni;
            }
      }

   return ret;
}
//----------------------------------------------------------------------------
bool
XML_node::matches(const XML_node * end_tag) const
{
   Assert(node_type == NT_start_tag);
   Assert(end_tag->node_type == NT_end_tag);

ShapeItem d = end_tag->src_pos + 2;   
   for (ShapeItem s = src_pos + 1;;)
       {
          const Unicode uni = src[s++];
          if (!is_name_char(uni))         return true;
          if (uni != end_tag->src[d++])   return false;
       }
}
//----------------------------------------------------------------------------
UCS_string
XML_node::get_tagname() const
{
size_t start = 0;
   if (node_type == NT_start_tag ||    // <TAGNAME ... >
       node_type == NT_leaf_tag)       // <TAGNAME ... />
      {
        start = src_pos + 1;   // char after <
      }
   else if (node_type == NT_end_tag)   // </TAGNAME
      {
        start = src_pos + 2;   // char after </
      }
   else
      {
        Q1(get_node_type_name())
        FIXME;
      }

size_t end = start;
   if (is_first_name_char(src[start]))   ++end;
   while (is_name_char(src[end]))        ++end;

   if (start == end)   // no valid tag name
      {
        MORE_ERROR() << "⎕XML: Bad tag name in: " << get_item();
        DOMAIN_ERROR;
      }

UCS_string ret;
   Assert(position >= 0);
   ret.append_number(position);
   ret.append(UCS_string(src, start, end - start));
   return ret;
}
//----------------------------------------------------------------------------
bool
XML_node::is_XML_char(Unicode uni)
{
   // XML standard chapter 2.2 "Characters"
   //
   if (uni > 0xD7FF)   // unlikely
      {
        return (uni  >= 0xE000  && uni <= 0xFFFD)
            || (uni  >= 0x10000 && uni <= 0x10FFFF);
      }

   if (uni < UNI_SPACE)   // occasionally
      {
         return uni == 0x0A   // most likely
             || uni == 0x09
             || uni == 0x0D;   // windows only
      }

   return true;
}
//----------------------------------------------------------------------------
bool
XML_node::is_name_char(Unicode uni)
{
   // XML standard chapter 2.3 "Common Syntactic Constructs"
   //
   if (is_first_name_char(uni))          return true;
   if (strchr("-.0123456789", uni))      return true;
   if (uni == 0xBF)                      return true;
   if (uni >= 0x0300 && uni <= 0x036F)   return true;
   if (uni >= 0x203F && uni <= 0x2040)   return true;
   return false;
}
//----------------------------------------------------------------------------
const int first_ranges[] =
{
     0xC0,    0xD6,
     0xD8,    0xF6,
     0xF8,   0x2FF,
    0x370,   0x37D,
    0x37F,  0x1FFF,
   0x200C,  0x200D,
   0x2070,  0x218F,
   0x2C00,  0x2FEF,
   0x3001,  0xD7FF,
   0xF900,  0xFDCF,
   0xFDF0,  0xFFFD,
  0x10000, 0xEFFFF
};

bool
XML_node::is_first_name_char(Unicode uni)
{
   // XML standard chapter 2.3 "Common Syntactic Constructs"
   //
   if (strchr(":ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              "_abcdefghijklmnopqrstuvwxyz", uni))   return true;

   for (size_t j = 0; j < sizeof(first_ranges); )
       {
         if (uni <  first_ranges[j++])   return false;   // below range start
         if (uni <= first_ranges[j++])   return true;    // below range end
       }

   return false;
}
//============================================================================
UCS_string
Quad_XML::skip_pos_prefix(const UCS_string & ucs)
{
ShapeItem pos = 1;
UCS_string ret;

   while (ucs[pos] >= UNI_0 && ucs[pos] <= UNI_9)   ++pos;   // skip digits
   while (pos < ucs.size())   ret += ucs[pos++];
   return ret;
}
//----------------------------------------------------------------------------
Token
Quad_XML::eval_AB(Value_P A, Value_P B) const
{
   if (A->get_rank() > 1)   RANK_ERROR;

const int function_number = A->get_cfirst().get_int_value();
   switch(function_number)
      {
         case 0:   // same as monadic ⎕XML
              {
                return eval_B(B);
              }

         case 1:   // read and convert an XML file
              {
                return convert_file(*B);
              }

         case 2:
              {
                Value_P Z = path_split(*B);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 3:
              {
                Value_P Z = name_split(*B);
                return Token(TOK_APL_VALUE1, Z);
              }

         case -3:
              {
                Value_P Z = name_unsplit(*B);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 4:
              {
                Value_P Z = tree(*B, tf_none);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 5:
              {
                Value_P Z = tree(*B, tf_with_pos | tf_with_decl);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 6:
              {
                Value_P Z = tree(*B, tf_with_pos | tf_fullpath);
                return Token(TOK_APL_VALUE1, Z);
              }

         case 7:
              {
                return all_members(*B, tf_all);
              }

         case 8:
              {
                return all_members(*B, tf_tagname);
              }

         case 9:
              {
                return all_members(*B, tf_decl);
              }

         case 10:
              {
                return all_members(*B, tf_text);
              }

         case 11:
              {
                return all_members(*B, tf_sub);
              }

         case 12:
              {
                return all_members(*B, tf_all | tf_flat);
              }

         case 13:
              {
                return all_members(*B, tf_sub | tf_flat);
              }

         case 14:
              {
                return next_member(*A, *B);
              }

      }

   MORE_ERROR() << "A ⎕XML B: Bad function number A=" << function_number;
   DOMAIN_ERROR;
}
//----------------------------------------------------------------------------
Token
Quad_XML::convert_file(const Value & B) const
{
   // safeguard against accidental use
   //
   if (B.is_structured())
      {
        MORE_ERROR() << "Bad B in 1 ⎕XML B (expecting filename)";
        DOMAIN_ERROR;
      }

const UCS_string filename_ucs(B);
const UTF8_string filename_utf(filename_ucs);

   errno = 0;
const int fd = open(filename_utf.c_str(), O_RDONLY);
   if (fd == -1)
      {
        MORE_ERROR() << "1 ⎕XML B: error reading " << B
                     << ": " << strerror(errno);
       DOMAIN_ERROR;
      }

struct stat st;
   if (fstat(fd, &st))
      {
        MORE_ERROR() << "1 ⎕XML B: error in fstat(" << B
                     << "): " << strerror(errno);
        ::close(fd);
        DOMAIN_ERROR;
      }

UTF8 * buffer = new UTF8[st.st_size];
   if (buffer == 0)
      {
        ::close(fd);
        WS_FULL;
      }

const ssize_t bytes_read = read(fd, buffer, st.st_size);
   if (bytes_read != st.st_size)
      {
        MORE_ERROR() << "1 ⎕XML B: error in reading " << B
                     << "): " << strerror(errno);
        ::close(fd);
        DOMAIN_ERROR;
        ::close(fd);
        delete [] buffer;
      }

const UTF8_string xml_document_utf8(buffer, bytes_read);
   delete[] buffer;
   ::close(fd);

UCS_string xml_document_ucs(xml_document_utf8);
Value_P xml_document_value(xml_document_ucs, LOC);

   return eval_B(xml_document_value);
}
//----------------------------------------------------------------------------
Value_P
Quad_XML::path_split(const Value & B)
{
   if (B.get_rank() != 1)       RANK_ERROR;

UCS_string path(B);

   // remove leading and trailing dots from path
   //
   while (path.size() && path.back() == UNI_FULLSTOP)   path.pop_back();
   while (path.size() && path[0]     == UNI_FULLSTOP)   path = path.drop(1);

ShapeItem len_Z = 1;

   // count number of dots
   loop(p, path.size())
       {
         if (path[p] == UNI_FULLSTOP)   ++len_Z;
       }

Value_P Z(len_Z, LOC);
ShapeItem from = 0;

   loop(p, path.size())
       {
         if (path[p] == UNI_FULLSTOP)
            {
              if (p == from)
                 {
                   while (Z->more())  Z->next_ravel_0();
                   Z->check_value(LOC);
                   MORE_ERROR() << "Empty member name in 2 ⎕XML";
                   LENGTH_ERROR;
                 }

              const UCS_string member(path, from, p - from);
              Value_P Zp(member, LOC);
              Z->next_ravel_Pointer(Zp.get());
              from = p + 1;
            }
       }

   if (from < path.size())
      {
        const UCS_string member(path, from, len_Z - from);
        Value_P Zp(member, LOC);
        Z->next_ravel_Pointer(Zp.get());
      }


   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Quad_XML::name_split(const Value & B)
{
   if (B.get_rank() != 1)       RANK_ERROR;
   if (B.element_count() < 3)   LENGTH_ERROR;

ShapeItem member_pos;     // the position in the XML file
Unicode category;
UCS_string name;
   split_name(&category, &member_pos, &name, B);

Value_P Z(3, LOC);
   Z->next_ravel_Char(category);
   Z->next_ravel_Int(member_pos);

Value_P Z3(name, LOC);
   Z->next_ravel_Pointer(Z3.get());
   Z->check_value(LOC);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Quad_XML::name_unsplit(const Value & B)
{
   if (B.get_rank() != 1)        RANK_ERROR;
   if (B.element_count() != 3)   LENGTH_ERROR;

const Cell & b0 =  B.get_cfirst();
const Cell & b1 =  B.get_cravel(1);
const Cell & b2 =  B.get_cravel(2);

const APL_Integer position = b1.get_int_value();
   if (position < Workspace::get_IO())
      {
         MORE_ERROR() << "¯3 ⎕XML B←NS POS NAME: POS is < ⎕IO";
         DOMAIN_ERROR;
      }

UCS_string UCS_Z;
   UCS_Z += b0.get_char_value();    // namespace (normally ∆, ⍙, or _)
   UCS_Z.append_number(position);   // position

   if (b2.is_character_cell())   // single char name: OK
      {
        UCS_Z += b2.get_char_value();
      }
   else                     // multi char name (the normal case)
      {
        const Value * B2 = b2.get_pointer_value().get();
        if (B2->get_rank() > 1)
           {
             MORE_ERROR() << "¯3 ⎕XML B←NS POS NAME: NAME has bad rank "
                          << B2->get_rank() << " (expected  1)";
             RANK_ERROR;
           }

         const ShapeItem len_B2 = B2->element_count();
         loop(bb, len_B2)   UCS_Z += B2->get_cravel(bb).get_char_value();
      }

Value_P Z(UCS_Z, LOC);
   return Z;
}
//----------------------------------------------------------------------------
Value_P
Quad_XML::tree(const Value & B, int flags)
{
   if (!B.is_structured())
      {
        MORE_ERROR() << "Non-structured B in A ⎕XML B";
        DOMAIN_ERROR;
      }

UCS_string z(UTF8_string("XML\n"));
UCS_string prefix;
const UCS_string name_prefix;
   tree(B, z, prefix, name_prefix, flags);

   return Value_P(z, LOC);
}
//----------------------------------------------------------------------------
int
Quad_XML::split_name(Unicode * category, ShapeItem * position,
                     UCS_string * name, const Value & value)
{
   // name is one of:
   // ⍙N                        (tag name, N was ⎕IO
   // ⍙NNNattribute_name        (attribute position NNN with name
   // ∆NNNsynmthetic_tag_name   (synthetic position NNN and tag name
   // _NNNuser_name             (user tag position NNN and tag name)

const ShapeItem len = value.element_count();
   Assert(len >= 1);
const Cell * src = &value.get_cfirst();
const Cell * end = src + len;

   // decode the category...
   //
const Unicode val0 = src++->get_char_value();
int ret = -1;
   if (val0 == UNI_DELTA_UNDERBAR)        ret = 0;
   else if (val0 == UNI_DELTA)            ret = 1;
   else if (val0 == UNI_UNDERSCORE)       ret = 2;
   else
      {
        MORE_ERROR() << "⎕XML: bad name '" << UCS_string() << "'; "
                        "it shall begin with ⍙ (name inside an XML tag),\n"
                        "    ∆ (synthetic name), or _ (tag name)";
        DOMAIN_ERROR;
      }

   // decode the position...
   //
ShapeItem pos = 0;
   for (;src < end; ++src)
       {
         const Unicode digit = src->get_char_value();
         if (digit >= UNI_0 && digit <= UNI_9)
            {
              pos = 10 * pos + digit - UNI_0;
            }
          else break;
       }

   // end of digits reached
   //
   if (category)   *category = val0;
   if (position)   *position = pos;
   if (name)   { while (src < end)   *name += src++->get_char_value(); }
   return ret;
}
//----------------------------------------------------------------------------
void
Quad_XML::tree(const Value & B, UCS_string & z, UCS_string & prefix,
               const UCS_string & name_prefix, int flags)
{
   enum { LEG_IND = 1,    // blanks before  ├──
          LEG_LEN = 3,    // number of ─ in ├──
          LEG_SPC = 1,    // blanks after   ├──
          LEG_TOT = LEG_LEN + LEG_SPC + LEG_IND
        };

std::vector<ShapeItem> member_indices;
   if (flags & tf_with_decl)   // all (no filter)
      B.sorted_members(member_indices, /* filter */ 0);
   else
      {
        const Unicode filter[] = { UNI_UNDERSCORE, Unicode_0 };
        B.sorted_members(member_indices, filter);
      }

UCS_string_vector member_names;
std::vector<const Cell *>member_values;

   loop(m, member_indices.size())
      {
        const Cell * cB = &B.get_cravel(2*member_indices[m]);
        Assert(cB->is_pointer_cell());
        if (flags & tf_with_pos)
           member_names.push_back(cB->get_pointer_value()->get_UCS_ravel());
        else
           member_names.push_back(skip_pos_prefix(cB->get_pointer_value()
                                                    ->get_UCS_ravel()));
        member_values.push_back(cB + 1);
      }

   // add an "empty" line to make z look nicer
   //
   if (member_indices.size())
   {
     // the continuation lines (if any) below the indentation above
     loop (p, prefix.size())
         {
           loop(l, LEG_IND)              z += UNI_SPACE;
           if (prefix[p] == UNI_SPACE)   z += UNI_SPACE;
           else                          z += UNI_LINE_VERT;
           loop(l, LEG_TOT - LEG_IND)    z += UNI_SPACE;
         }
     loop(l, LEG_IND)                    z += UNI_SPACE;
     z += UNI_LINE_VERT;
     z += UNI_LF;
   }

   prefix += UNI_LINE_VERT_RIGHT;   // add ├
   loop(m, member_names.size())
      {
        loop(l, LEG_IND)              z += UNI_SPACE;
        Unicode last_char = prefix.back();
        const bool last_member = m == (member_names.size() - 1);
        if (last_member)
           {
             last_char     = UNI_LINE_UP_RIGHT;   // └ in this line
             prefix.back() = UNI_SPACE;           // ' ' in subsequent lines
           }

        loop(p, (prefix.size() - 1))
            {
              if (prefix[p] == UNI_SPACE)   z += UNI_SPACE;
              else                          z += UNI_LINE_VERT;
              loop(l, LEG_TOT)   z += UNI_SPACE;
            }
        z += last_char;
        loop(l, LEG_LEN)   z += UNI_LINE_HORI;
        z += UNI_SPACE;
        if (flags & tf_fullpath)
           {
             z += name_prefix;
             z += UNI_FULLSTOP;
           }

        z += member_names[m];
        z += UNI_LF;
        if (!member_values[m]->is_pointer_cell())   continue;

        Value_P sub = member_values[m]->get_pointer_value();
        if (!sub->is_structured())   continue;
        UCS_string subname(name_prefix);
        subname += UNI_FULLSTOP;
        subname += member_names[m];
        tree(*sub, z, prefix, subname, flags);
        if (last_member && prefix.size() > 1)
           {
             // the char was │ initially, has now become └, and should be
             // blank in subsequent lines.
             //
             prefix[prefix.size() - 2] = UNI_SPACE;
           }
      }
   prefix.pop_back();
}
//----------------------------------------------------------------------------
Token
Quad_XML::all_members(const Value & B, int flags)
{
   if (!B.is_structured())
      {
        MORE_ERROR() << "Non-structured B in A ⎕XML B";
        DOMAIN_ERROR;
      }

const UCS_string name_prefix;
UCS_string_vector result;
   result.reserve(B.get_all_members_count());

   all_members(result, B, name_prefix, flags);
Value_P Z(result.size(), LOC);
   loop(r, result.size())
       {
          Value_P Zr(result[r], LOC);
          Z->next_ravel_Pointer(Zr.get());
       }

   Z->check_value(LOC);
   return Token(TOK_APL_VALUE1, Z);
}
//----------------------------------------------------------------------------
void
Quad_XML::all_members(UCS_string_vector & result, const Value & B,
                      const UCS_string & name_prefix, int flags)
{
Unicode filters[4];
int f = 0;
   if (flags & tf_tagname)   filters[f++] = UNI_DELTA_UNDERBAR;
   if (flags & tf_delta)     filters[f++] = UNI_DELTA;
   filters[f++] = UNI_UNDERSCORE;   // always return subnodes
   filters[f++] = Unicode_0;

std::vector<ShapeItem> member_indices;
   B.sorted_members(member_indices, filters);

UCS_string_vector member_names;
std::vector<const Cell *>member_values;
   loop(m, member_indices.size())
      {
        const Cell & cB = B.get_cravel(2*member_indices[m]);
        Assert(cB.is_pointer_cell());
        const UCS_string member_name = cB.get_pointer_value()->get_UCS_ravel();
        member_names.push_back(member_name);
        member_values.push_back(&cB + 1);
      }

   loop(m, member_names.size())
      {
        UCS_string path(name_prefix);
        path += UNI_FULLSTOP;
        path.append(member_names[m]);

        // see if the path is desired in the result. In most cases the
        // filter has suppressed them, so we only need to check the special
       // cases where we have overridden the filter
        //
        if (member_names[m].contains(UNI_DELTA_UNDERBAR))
           {
             result.push_back(path);
           }
        else if (member_names[m].contains(UNI_DELTA))
           {
             if (member_names[m].ends_with("text"))   // ∆text
                {
                  if (flags & tf_text) result.push_back(path);
                }
             else
                {
                  if (flags & tf_decl) result.push_back(path);
                }
           }
        else if (member_names[m].contains(UNI_UNDERSCORE))
           {
             if (flags & tf_sub)   result.push_back(path);
           }
        else FIXME;

        if (flags & tf_flat)   continue;                 // don't recurse

        const Cell & member_data = *member_values[m];
        if (!member_data.is_pointer_cell())   continue;  // can't recurse

        const Value & sub = *member_data.get_pointer_value();
        if (sub.is_structured())   all_members(result, sub, path, flags);
      }
}
//----------------------------------------------------------------------------
Token
Quad_XML::next_member(const Value & A, const Value & B)
{
   // A is (14, ⊂A1)
   // B is an associative array;
   // Return the next member after A1 (e.g. the unstructured text after tag A1)
   //
   if (!B.is_structured())
      {
        MORE_ERROR() << "Non-structured B in 14 ⎕XML B";
        DOMAIN_ERROR;
      }

const ShapeItem B_total = B.get_all_members_count();

   if (A.get_rank() != 1)        RANK_ERROR;
   if (A.element_count() != 2)
      {
        if (A.element_count() < 2)
           MORE_ERROR() << "too few items in left argument of (14 A) ⎕XML B";
        else
           MORE_ERROR() << "too many items in left argument of (14 A) ⎕XML B";
        LENGTH_ERROR; 
      }

const Value & A1 = *A.get_cravel(1).get_pointer_value();
   if (A1.element_count() == 0)
      {
        // an empty A1 shall return the smallest element
        //
        std::vector<ShapeItem> member_indices;
        B.sorted_members(member_indices, /* filters */ 0);

        const Cell & cell = B.get_cravel(2*member_indices[0]);
        Value * name =  cell.get_pointer_value().get();
        return Token(TOK_APL_VALUE1, CLONE(name, LOC));
      }

Value_P path = path_split(A1);
const ShapeItem path_length = path->element_count();
   if (path_length < 1)
      {
        MORE_ERROR() << "(14 A) ⎕XML B: too few path elements in A";
        LENGTH_ERROR; 
      }

   // walk along path to find the value that contains the final item in the
   // path. path_idx is the position in the path, corresponding to the
   // depth in the value tree.
   //
const Value * container = &B;
   for (size_t path_idx = 0; path_idx < size_t(path_length - 1); ++path_idx)
       {
         const UCS_string member(path->get_cravel(path_idx));
         const Cell * data_cell = container->get_member_data(member);
         if (data_cell == 0)
            {
              MORE_ERROR() << "(14 PATH) ⎕XML: could not find the "
                           << (path_idx+1) << ". member '" << member
                           << "' of PATH (= '" << UCS_string(A1) << "')";
              DOMAIN_ERROR;
            }

         container = data_cell->get_pointer_value().get();
         if (!container->is_structured())
            {
              MORE_ERROR() << "(14 A) ⎕XML: member " << member
                           << " of path A (" << UCS_string(A1)
                           << " exists, but is not a structured value)";
              DOMAIN_ERROR;
            }
       }
const Value & last_path_item = path_length == 1
                             ? A1
                             : *path->get_cravel(path_length - 1)
                                    .get_pointer_value();
ShapeItem leaf_position;
   {
     const int weight = split_name(0, &leaf_position, 0, last_path_item);
     leaf_position += weight * B_total;
   }

std::vector<ShapeItem> member_indices;
   container->sorted_members(member_indices, /* filters */ 0);

   loop(m, member_indices.size())
      {
        const Cell & cell = container->get_cravel(2*member_indices[m]);
        Assert(cell.is_pointer_cell());

        Value * name_m =  cell.get_pointer_value().get();
        ShapeItem position_m;
        const int weight = split_name(0, &position_m, 0, *name_m);
        position_m += weight*B_total;

        if (position_m > leaf_position)   // next item after leaf
           {
             return Token(TOK_APL_VALUE1, CLONE(name_m, LOC));
           }
      }

   return Token(TOK_APL_VALUE1, Str0(LOC));   // no next item: return ""
}
//============================================================================
