<?

$demogroupname = "Xmi";

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

XMI files are XML files designed for the interchange of data. We address here a
special kind of XMI files, those representing UML, but the same lens we are
using could be used as well for every kind of XMI files.

As you may see in the window below, the XMI files are somewhat verbose and it is
neither easy to edit them manually, nor to understand at a first glance what are
their contents. The file below is a single model containing a single class
<tt>MyClass</tt> with a few members, and two native datatypes <tt>int</tt> and
<tt>void</tt>. Here are two screenshots from ArgoUML, an UML editor, showing the 
corresponding hierarchy tree and class diagram.
<p align="center">
<img src="images/MyModel.gif"/> <img src="images/MyClass.gif"/>
</p>

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<?xml version="1.0" encoding="UTF-8"?>
<XMI xmi.version="1.0">
  <XMI.header>
    <XMI.documentation>
      <XMI.exporter>Novosoft UML Library</XMI.exporter>
      <XMI.exporterVersion>0.4.20</XMI.exporterVersion>
    </XMI.documentation>
    <XMI.metamodel xmi.name="UML" xmi.version="1.3"/>
  </XMI.header>
  <XMI.content>
    <Model_Management.Model xmi.id="xmi.1" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-8000">
      <Foundation.Core.ModelElement.name>myModel</Foundation.Core.ModelElement.name>
      <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
      <Foundation.Core.GeneralizableElement.isRoot xmi.value="false"/>
      <Foundation.Core.GeneralizableElement.isLeaf xmi.value="false"/>
      <Foundation.Core.GeneralizableElement.isAbstract xmi.value="false"/>
      <Foundation.Core.Namespace.ownedElement>
        <Foundation.Core.Class xmi.id="xmi.2" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7ffc">
          <Foundation.Core.ModelElement.name>MyClass</Foundation.Core.ModelElement.name>
          <Foundation.Core.ModelElement.visibility xmi.value="public"/>
          <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isRoot xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isLeaf xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isAbstract xmi.value="false"/>
          <Foundation.Core.Class.isActive xmi.value="false"/>
          <Foundation.Core.ModelElement.namespace>
            <Foundation.Core.Namespace xmi.idref="xmi.1"/>
          </Foundation.Core.ModelElement.namespace>
          <Foundation.Core.Classifier.feature>
            <Foundation.Core.Attribute xmi.id="xmi.3" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7ff3">
              <Foundation.Core.ModelElement.name>myStaticAttribute</Foundation.Core.ModelElement.name>
              <Foundation.Core.ModelElement.visibility xmi.value="public"/>
              <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
              <Foundation.Core.Feature.ownerScope xmi.value="classifier"/>
              <Foundation.Core.StructuralFeature.multiplicity>
                <Foundation.Data_Types.Multiplicity xmi.id="xmi.4">
                  <Foundation.Data_Types.Multiplicity.range>
                    <Foundation.Data_Types.MultiplicityRange xmi.id="xmi.5">
                      <Foundation.Data_Types.MultiplicityRange.lower>1</Foundation.Data_Types.MultiplicityRange.lower>
                      <Foundation.Data_Types.MultiplicityRange.upper>1</Foundation.Data_Types.MultiplicityRange.upper>
                    </Foundation.Data_Types.MultiplicityRange>
                  </Foundation.Data_Types.Multiplicity.range>
                </Foundation.Data_Types.Multiplicity>
              </Foundation.Core.StructuralFeature.multiplicity>
              <Foundation.Core.StructuralFeature.changeability xmi.value="changeable"/>
              <Foundation.Core.StructuralFeature.targetScope xmi.value="instance"/>
              <Foundation.Core.Attribute.initialValue>
                <Foundation.Data_Types.Expression xmi.id="xmi.6">
                  <Foundation.Data_Types.Expression.language>Java</Foundation.Data_Types.Expression.language>
                  <Foundation.Data_Types.Expression.body></Foundation.Data_Types.Expression.body>
                </Foundation.Data_Types.Expression>
              </Foundation.Core.Attribute.initialValue>
              <Foundation.Core.Feature.owner>
                <Foundation.Core.Classifier xmi.idref="xmi.2"/>
              </Foundation.Core.Feature.owner>
              <Foundation.Core.ModelElement.taggedValue>
                <Foundation.Extension_Mechanisms.TaggedValue xmi.id="xmi.7">
                  <Foundation.Extension_Mechanisms.TaggedValue.tag>transient</Foundation.Extension_Mechanisms.TaggedValue.tag>
                  <Foundation.Extension_Mechanisms.TaggedValue.value>false</Foundation.Extension_Mechanisms.TaggedValue.value>
                  <Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                    <Foundation.Core.ModelElement xmi.idref="xmi.3"/>
                  </Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                </Foundation.Extension_Mechanisms.TaggedValue>
                <Foundation.Extension_Mechanisms.TaggedValue xmi.id="xmi.8">
                  <Foundation.Extension_Mechanisms.TaggedValue.tag>volatile</Foundation.Extension_Mechanisms.TaggedValue.tag>
                  <Foundation.Extension_Mechanisms.TaggedValue.value>false</Foundation.Extension_Mechanisms.TaggedValue.value>
                  <Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                    <Foundation.Core.ModelElement xmi.idref="xmi.3"/>
                  </Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                </Foundation.Extension_Mechanisms.TaggedValue>
              </Foundation.Core.ModelElement.taggedValue>
            </Foundation.Core.Attribute>
            <Foundation.Core.Attribute xmi.id="xmi.9" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7ff0">
              <Foundation.Core.ModelElement.name>myDynamicAttribute</Foundation.Core.ModelElement.name>
              <Foundation.Core.ModelElement.visibility xmi.value="public"/>
              <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
              <Foundation.Core.Feature.ownerScope xmi.value="instance"/>
              <Foundation.Core.StructuralFeature.multiplicity>
                <Foundation.Data_Types.Multiplicity xmi.idref="xmi.4"/>
              </Foundation.Core.StructuralFeature.multiplicity>
              <Foundation.Core.StructuralFeature.changeability xmi.value="changeable"/>
              <Foundation.Core.StructuralFeature.targetScope xmi.value="instance"/>
              <Foundation.Core.Feature.owner>
                <Foundation.Core.Classifier xmi.idref="xmi.2"/>
              </Foundation.Core.Feature.owner>
              <Foundation.Core.ModelElement.taggedValue>
                <Foundation.Extension_Mechanisms.TaggedValue xmi.id="xmi.10">
                  <Foundation.Extension_Mechanisms.TaggedValue.tag>transient</Foundation.Extension_Mechanisms.TaggedValue.tag>
                  <Foundation.Extension_Mechanisms.TaggedValue.value>false</Foundation.Extension_Mechanisms.TaggedValue.value>
                  <Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                    <Foundation.Core.ModelElement xmi.idref="xmi.9"/>
                  </Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                </Foundation.Extension_Mechanisms.TaggedValue>
                <Foundation.Extension_Mechanisms.TaggedValue xmi.id="xmi.11">
                  <Foundation.Extension_Mechanisms.TaggedValue.tag>volatile</Foundation.Extension_Mechanisms.TaggedValue.tag>
                  <Foundation.Extension_Mechanisms.TaggedValue.value>false</Foundation.Extension_Mechanisms.TaggedValue.value>
                  <Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                    <Foundation.Core.ModelElement xmi.idref="xmi.9"/>
                  </Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                </Foundation.Extension_Mechanisms.TaggedValue>
              </Foundation.Core.ModelElement.taggedValue>
            </Foundation.Core.Attribute>
            <Foundation.Core.Operation xmi.id="xmi.12" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7fee">
              <Foundation.Core.ModelElement.name>myMethod</Foundation.Core.ModelElement.name>
              <Foundation.Core.ModelElement.visibility xmi.value="public"/>
              <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
              <Foundation.Core.Feature.ownerScope xmi.value="instance"/>
              <Foundation.Core.BehavioralFeature.isQuery xmi.value="false"/>
              <Foundation.Core.Operation.concurrency xmi.value="sequential"/>
              <Foundation.Core.Operation.isRoot xmi.value="false"/>
              <Foundation.Core.Operation.isLeaf xmi.value="false"/>
              <Foundation.Core.Operation.isAbstract xmi.value="false"/>
              <Foundation.Core.Feature.owner>
                <Foundation.Core.Classifier xmi.idref="xmi.2"/>
              </Foundation.Core.Feature.owner>
              <Foundation.Core.BehavioralFeature.parameter>
                <Foundation.Core.Parameter xmi.id="xmi.13" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7fed">
                  <Foundation.Core.ModelElement.name>return</Foundation.Core.ModelElement.name>
                  <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
                  <Foundation.Core.Parameter.kind xmi.value="return"/>
                  <Foundation.Core.Parameter.behavioralFeature>
                    <Foundation.Core.BehavioralFeature xmi.idref="xmi.12"/>
                  </Foundation.Core.Parameter.behavioralFeature>
                  <Foundation.Core.Parameter.type>
                    <Foundation.Core.Classifier xmi.idref="xmi.14"/>
                  </Foundation.Core.Parameter.type>
                </Foundation.Core.Parameter>
                <Foundation.Core.Parameter xmi.id="xmi.15" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7fec">
                  <Foundation.Core.ModelElement.name>myArgument</Foundation.Core.ModelElement.name>
                  <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
                  <Foundation.Core.Parameter.kind xmi.value="in"/>
                  <Foundation.Core.Parameter.behavioralFeature>
                    <Foundation.Core.BehavioralFeature xmi.idref="xmi.12"/>
                  </Foundation.Core.Parameter.behavioralFeature>
                  <Foundation.Core.Parameter.type>
                    <Foundation.Core.Classifier xmi.idref="xmi.16"/>
                  </Foundation.Core.Parameter.type>
                </Foundation.Core.Parameter>
              </Foundation.Core.BehavioralFeature.parameter>
            </Foundation.Core.Operation>
          </Foundation.Core.Classifier.feature>
        </Foundation.Core.Class>
        <Foundation.Core.DataType xmi.id="xmi.14" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7fef">
          <Foundation.Core.ModelElement.name>void</Foundation.Core.ModelElement.name>
          <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isRoot xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isLeaf xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isAbstract xmi.value="false"/>
          <Foundation.Core.ModelElement.namespace>
            <Foundation.Core.Namespace xmi.idref="xmi.1"/>
          </Foundation.Core.ModelElement.namespace>
        </Foundation.Core.DataType>
        <Foundation.Core.DataType xmi.id="xmi.16" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7feb">
          <Foundation.Core.ModelElement.name>int</Foundation.Core.ModelElement.name>
          <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isRoot xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isLeaf xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isAbstract xmi.value="false"/>
          <Foundation.Core.ModelElement.namespace>
            <Foundation.Core.Namespace xmi.idref="xmi.1"/>
          </Foundation.Core.ModelElement.namespace>
        </Foundation.Core.DataType>
      </Foundation.Core.Namespace.ownedElement>
    </Model_Management.Model>
  </XMI.content>
</XMI>
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "xmi";
$demo["r2format"] = "xmi";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

The goal of this demo is to illustrate the meta encoding of XMI
files. Since XMI files can contain many different tags (the DTD
specification of the encoding of UML files in XMI is about 100 pages
!), we cannot provide a lens, nor a synchronization schema, that knows
about the whole structure of the file and about every name (at least,
we cannot <i>manually</i>, but we do not have a DTD -> lens generator
for now). Therefore, we just separate the different parts of the
elements by testing their children against appropriate schemas. 
<p>
Indeed, an XMI-UML element may look like this :
<p><tt>
&lt;Foundation.Core.DataType xmi.id=<font color="#BB0000">"xmi.16"</font> xmi.uuid=<font color="#00BB00">"127-0-0-1-587175e0:105a31639e2:-7feb"</font>&gt;<br>
&nbsp;&lt;<font color="#00BB00">Foundation.Core.ModelElement.name</font>&gt;<font color="#0000BB">int</font>&lt;/Foundation.Core.ModelElement.name&gt;<br>
&nbsp;&lt;<font color="#00BB00">Foundation.Core.ModelElement.isSpecification</font> xmi.value=<font color="#0000BB">"false"</font>/&gt;<br>
&nbsp;&lt;<font color="#00BB00">Foundation.Core.GeneralizableElement.isRoot</font> xmi.value=<font color="#0000BB">"false"</font>/&gt;<br>
&nbsp;&lt;<font color="#00BB00">Foundation.Core.GeneralizableElement.isLeaf</font> xmi.value=<font color="#0000BB">"false"</font>/&gt;<br>
&nbsp;&lt;<font color="#00BB00">Foundation.Core.GeneralizableElement.isAbstract</font> xmi.value=<font color="#0000BB">"false"</font>/&gt;<br>
&nbsp;&lt;Foundation.Core.ModelElement.namespace&gt;<br>
&nbsp;&nbsp;&lt;Foundation.Core.Namespace xmi.idref=<font color="#BB0000">"xmi.1"</font>/&gt;<br>
&nbsp;&lt;/Foundation.Core.ModelElement.namespace&gt;<br>
&lt;/Foundation.Core.DataType&gt;<br></tt>
<p>
The stuff in green are keys, that means they are going to be used
for aligning the contents, or values, which are highlighted in blue in the view above.
Also, there are values that are local and that we do not keep in the abstract view; these
are highlighted in red.
The corresponding encoding will plunge all the contents under an
unique identifier (the <tt>xmi.uuid</tt> attribute), and reorganize
the contents according to what they look like, that is, tags with a
<tt>xmi.value</tt> attribute as <tt>values</tt>, tags leading to some
<tt>PCDATA</tt> as <tt>pcdatas</tt>, and so on. The abstract view
corresponding to this snippet of XMI is :
<p><tt>
{<font color="#00BB00">"127-0-0-1-587175e0:105a31639e2:-7feb"</font> =<br>
&nbsp;{owned-elts = {},<br>
&nbsp;&nbsp;pcdatas = {<font color="#00BB00">"Foundation.Core.ModelElement.name"</font> = {<font color="#0000BB">int</font>}},<br>
&nbsp;&nbsp;values =<br>
&nbsp;&nbsp;&nbsp;{<font color="#00BB00">"Foundation.Core.GeneralizableElement.isAbstract"</font> = {<font color="#0000BB">false</font>},<br>
&nbsp;&nbsp;&nbsp;&nbsp;<font color="#00BB00">"Foundation.Core.GeneralizableElement.isLeaf"</font> = {<font color="#0000BB">false</font>},<br>
&nbsp;&nbsp;&nbsp;&nbsp;<font color="#00BB00">"Foundation.Core.GeneralizableElement.isRoot"</font> = {<font color="#0000BB">false</font>},<br>
&nbsp;&nbsp;&nbsp;&nbsp;<font color="#00BB00">"Foundation.Core.ModelElement.isSpecification"</font> = {<font color="#0000BB">false</font>}}}}<br>
<p></tt>

<tt>owned-elts</tt> is used to store elements in containers, and since
this datatype does not contain any other elements, it is just pointing
to the empty tree. Finally, the
<tt>Foundation.Core.ModelElement.namespace</tt> tag of the concrete
view and its <tt>xmi.id*</tt> properties has been filtered away,
because this is information that is local to the file and therefore,
we do not want to synchronize it.

Try changing values and/or keys in the abstract view below 
(or in the concrete view if you are brave enough :)) and see
how everything gets synchronized by Harmony.

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<?xml version="1.0" encoding="UTF-8"?>
<XMI xmi.version="1.0">
  <XMI.header>
    <XMI.documentation>
      <XMI.exporter>Novosoft UML Library</XMI.exporter>
      <XMI.exporterVersion>0.4.20</XMI.exporterVersion>
    </XMI.documentation>
    <XMI.metamodel xmi.name="UML" xmi.version="1.3"/>
  </XMI.header>
  <XMI.content>
    <Model_Management.Model xmi.id="xmi.1" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-8000">
      <Foundation.Core.ModelElement.name>myModel</Foundation.Core.ModelElement.name>
      <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
      <Foundation.Core.GeneralizableElement.isRoot xmi.value="false"/>
      <Foundation.Core.GeneralizableElement.isLeaf xmi.value="false"/>
      <Foundation.Core.GeneralizableElement.isAbstract xmi.value="false"/>
      <Foundation.Core.Namespace.ownedElement>
        <Foundation.Core.Class xmi.id="xmi.2" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7ffc">
          <Foundation.Core.ModelElement.name>MyClass</Foundation.Core.ModelElement.name>
          <Foundation.Core.ModelElement.visibility xmi.value="public"/>
          <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isRoot xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isLeaf xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isAbstract xmi.value="false"/>
          <Foundation.Core.Class.isActive xmi.value="false"/>
          <Foundation.Core.ModelElement.namespace>
            <Foundation.Core.Namespace xmi.idref="xmi.1"/>
          </Foundation.Core.ModelElement.namespace>
          <Foundation.Core.Classifier.feature>
            <Foundation.Core.Attribute xmi.id="xmi.3" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7ff3">
              <Foundation.Core.ModelElement.name>myStaticAttribute</Foundation.Core.ModelElement.name>
              <Foundation.Core.ModelElement.visibility xmi.value="public"/>
              <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
              <Foundation.Core.Feature.ownerScope xmi.value="classifier"/>
              <Foundation.Core.StructuralFeature.multiplicity>
                <Foundation.Data_Types.Multiplicity xmi.id="xmi.4">
                  <Foundation.Data_Types.Multiplicity.range>
                    <Foundation.Data_Types.MultiplicityRange xmi.id="xmi.5">
                      <Foundation.Data_Types.MultiplicityRange.lower>1</Foundation.Data_Types.MultiplicityRange.lower>
                      <Foundation.Data_Types.MultiplicityRange.upper>1</Foundation.Data_Types.MultiplicityRange.upper>
                    </Foundation.Data_Types.MultiplicityRange>
                  </Foundation.Data_Types.Multiplicity.range>
                </Foundation.Data_Types.Multiplicity>
              </Foundation.Core.StructuralFeature.multiplicity>
              <Foundation.Core.StructuralFeature.changeability xmi.value="changeable"/>
              <Foundation.Core.StructuralFeature.targetScope xmi.value="instance"/>
              <Foundation.Core.Attribute.initialValue>
                <Foundation.Data_Types.Expression xmi.id="xmi.6">
                  <Foundation.Data_Types.Expression.language>Java</Foundation.Data_Types.Expression.language>
                  <Foundation.Data_Types.Expression.body></Foundation.Data_Types.Expression.body>
                </Foundation.Data_Types.Expression>
              </Foundation.Core.Attribute.initialValue>
              <Foundation.Core.Feature.owner>
                <Foundation.Core.Classifier xmi.idref="xmi.2"/>
              </Foundation.Core.Feature.owner>
              <Foundation.Core.ModelElement.taggedValue>
                <Foundation.Extension_Mechanisms.TaggedValue xmi.id="xmi.7">
                  <Foundation.Extension_Mechanisms.TaggedValue.tag>transient</Foundation.Extension_Mechanisms.TaggedValue.tag>
                  <Foundation.Extension_Mechanisms.TaggedValue.value>false</Foundation.Extension_Mechanisms.TaggedValue.value>
                  <Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                    <Foundation.Core.ModelElement xmi.idref="xmi.3"/>
                  </Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                </Foundation.Extension_Mechanisms.TaggedValue>
                <Foundation.Extension_Mechanisms.TaggedValue xmi.id="xmi.8">
                  <Foundation.Extension_Mechanisms.TaggedValue.tag>volatile</Foundation.Extension_Mechanisms.TaggedValue.tag>
                  <Foundation.Extension_Mechanisms.TaggedValue.value>false</Foundation.Extension_Mechanisms.TaggedValue.value>
                  <Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                    <Foundation.Core.ModelElement xmi.idref="xmi.3"/>
                  </Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                </Foundation.Extension_Mechanisms.TaggedValue>
              </Foundation.Core.ModelElement.taggedValue>
            </Foundation.Core.Attribute>
            <Foundation.Core.Attribute xmi.id="xmi.9" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7ff0">
              <Foundation.Core.ModelElement.name>myDynamicAttribute</Foundation.Core.ModelElement.name>
              <Foundation.Core.ModelElement.visibility xmi.value="public"/>
              <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
              <Foundation.Core.Feature.ownerScope xmi.value="instance"/>
              <Foundation.Core.StructuralFeature.multiplicity>
                <Foundation.Data_Types.Multiplicity xmi.idref="xmi.4"/>
              </Foundation.Core.StructuralFeature.multiplicity>
              <Foundation.Core.StructuralFeature.changeability xmi.value="changeable"/>
              <Foundation.Core.StructuralFeature.targetScope xmi.value="instance"/>
              <Foundation.Core.Feature.owner>
                <Foundation.Core.Classifier xmi.idref="xmi.2"/>
              </Foundation.Core.Feature.owner>
              <Foundation.Core.ModelElement.taggedValue>
                <Foundation.Extension_Mechanisms.TaggedValue xmi.id="xmi.10">
                  <Foundation.Extension_Mechanisms.TaggedValue.tag>transient</Foundation.Extension_Mechanisms.TaggedValue.tag>
                  <Foundation.Extension_Mechanisms.TaggedValue.value>false</Foundation.Extension_Mechanisms.TaggedValue.value>
                  <Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                    <Foundation.Core.ModelElement xmi.idref="xmi.9"/>
                  </Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                </Foundation.Extension_Mechanisms.TaggedValue>
                <Foundation.Extension_Mechanisms.TaggedValue xmi.id="xmi.11">
                  <Foundation.Extension_Mechanisms.TaggedValue.tag>volatile</Foundation.Extension_Mechanisms.TaggedValue.tag>
                  <Foundation.Extension_Mechanisms.TaggedValue.value>false</Foundation.Extension_Mechanisms.TaggedValue.value>
                  <Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                    <Foundation.Core.ModelElement xmi.idref="xmi.9"/>
                  </Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                </Foundation.Extension_Mechanisms.TaggedValue>
              </Foundation.Core.ModelElement.taggedValue>
            </Foundation.Core.Attribute>
            <Foundation.Core.Operation xmi.id="xmi.12" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7fee">
              <Foundation.Core.ModelElement.name>myMethod</Foundation.Core.ModelElement.name>
              <Foundation.Core.ModelElement.visibility xmi.value="public"/>
              <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
              <Foundation.Core.Feature.ownerScope xmi.value="instance"/>
              <Foundation.Core.BehavioralFeature.isQuery xmi.value="false"/>
              <Foundation.Core.Operation.concurrency xmi.value="sequential"/>
              <Foundation.Core.Operation.isRoot xmi.value="false"/>
              <Foundation.Core.Operation.isLeaf xmi.value="false"/>
              <Foundation.Core.Operation.isAbstract xmi.value="false"/>
              <Foundation.Core.Feature.owner>
                <Foundation.Core.Classifier xmi.idref="xmi.2"/>
              </Foundation.Core.Feature.owner>
              <Foundation.Core.BehavioralFeature.parameter>
                <Foundation.Core.Parameter xmi.id="xmi.13" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7fed">
                  <Foundation.Core.ModelElement.name>return</Foundation.Core.ModelElement.name>
                  <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
                  <Foundation.Core.Parameter.kind xmi.value="return"/>
                  <Foundation.Core.Parameter.behavioralFeature>
                    <Foundation.Core.BehavioralFeature xmi.idref="xmi.12"/>
                  </Foundation.Core.Parameter.behavioralFeature>
                  <Foundation.Core.Parameter.type>
                    <Foundation.Core.Classifier xmi.idref="xmi.14"/>
                  </Foundation.Core.Parameter.type>
                </Foundation.Core.Parameter>
                <Foundation.Core.Parameter xmi.id="xmi.15" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7fec">
                  <Foundation.Core.ModelElement.name>myArgument</Foundation.Core.ModelElement.name>
                  <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
                  <Foundation.Core.Parameter.kind xmi.value="in"/>
                  <Foundation.Core.Parameter.behavioralFeature>
                    <Foundation.Core.BehavioralFeature xmi.idref="xmi.12"/>
                  </Foundation.Core.Parameter.behavioralFeature>
                  <Foundation.Core.Parameter.type>
                    <Foundation.Core.Classifier xmi.idref="xmi.16"/>
                  </Foundation.Core.Parameter.type>
                </Foundation.Core.Parameter>
              </Foundation.Core.BehavioralFeature.parameter>
            </Foundation.Core.Operation>
          </Foundation.Core.Classifier.feature>
        </Foundation.Core.Class>
        <Foundation.Core.DataType xmi.id="xmi.14" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7fef">
          <Foundation.Core.ModelElement.name>void</Foundation.Core.ModelElement.name>
          <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isRoot xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isLeaf xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isAbstract xmi.value="false"/>
          <Foundation.Core.ModelElement.namespace>
            <Foundation.Core.Namespace xmi.idref="xmi.1"/>
          </Foundation.Core.ModelElement.namespace>
        </Foundation.Core.DataType>
        <Foundation.Core.DataType xmi.id="xmi.16" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7feb">
          <Foundation.Core.ModelElement.name>int</Foundation.Core.ModelElement.name>
          <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isRoot xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isLeaf xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isAbstract xmi.value="false"/>
          <Foundation.Core.ModelElement.namespace>
            <Foundation.Core.Namespace xmi.idref="xmi.1"/>
          </Foundation.Core.ModelElement.namespace>
        </Foundation.Core.DataType>
      </Foundation.Core.Namespace.ownedElement>
    </Model_Management.Model>
  </XMI.content>
</XMI>

XXX;
# ---------------------------------------------------------
$demo["r1format"] = "xmi";
$demo["r2format"] = "meta";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX

The lens and schema used to synchronize these files are very optimistic,
in the sense that they are not very knowledgable about the whole
structure and the subtleties of the encoding of UML in XMI, but relies
mainly on patterns to go through the structure and reorganize everything.
<p>
This is rather impressing that such an easy lens can do such a job without
knowing much about what it is dealing with, by opposition to the iCalendar demo,
where the lens and the schema are completely accurate and following the RFC
specification. The only way to get a sense of what this lens is able to do, is
to actually play with the files with an UML editor, and not manually.
<p>
Therefore, we enjoin you to grab Argo <a href="http://argouml.tigris.org/">here</a>
or to use you favorite UML editor, if you have one, edit the 
<a href="../examples/xmi/archive.xmi">example file</a> as you wish, paste edited versions of the file into the first and second replicas, press Synchronize, and see what happens! 
Please let us know of any trouble
you may have and any misbehaviour of the synchronization that you may encounter,
especially if you are using something else than Argo.

XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
<?xml version="1.0" encoding="UTF-8"?>
<XMI xmi.version="1.0">
  <XMI.header>
    <XMI.documentation>
      <XMI.exporter>Novosoft UML Library</XMI.exporter>
      <XMI.exporterVersion>0.4.20</XMI.exporterVersion>
    </XMI.documentation>
    <XMI.metamodel xmi.name="UML" xmi.version="1.3"/>
  </XMI.header>
  <XMI.content>
    <Model_Management.Model xmi.id="xmi.1" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-8000">
      <Foundation.Core.ModelElement.name>myModel</Foundation.Core.ModelElement.name>
      <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
      <Foundation.Core.GeneralizableElement.isRoot xmi.value="false"/>
      <Foundation.Core.GeneralizableElement.isLeaf xmi.value="false"/>
      <Foundation.Core.GeneralizableElement.isAbstract xmi.value="false"/>
      <Foundation.Core.Namespace.ownedElement>
        <Foundation.Core.Class xmi.id="xmi.2" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7ffc">
          <Foundation.Core.ModelElement.name>MyClass</Foundation.Core.ModelElement.name>
          <Foundation.Core.ModelElement.visibility xmi.value="public"/>
          <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isRoot xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isLeaf xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isAbstract xmi.value="false"/>
          <Foundation.Core.Class.isActive xmi.value="false"/>
          <Foundation.Core.ModelElement.namespace>
            <Foundation.Core.Namespace xmi.idref="xmi.1"/>
          </Foundation.Core.ModelElement.namespace>
          <Foundation.Core.Classifier.feature>
            <Foundation.Core.Attribute xmi.id="xmi.3" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7ff3">
              <Foundation.Core.ModelElement.name>myStaticAttribute</Foundation.Core.ModelElement.name>
              <Foundation.Core.ModelElement.visibility xmi.value="public"/>
              <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
              <Foundation.Core.Feature.ownerScope xmi.value="classifier"/>
              <Foundation.Core.StructuralFeature.multiplicity>
                <Foundation.Data_Types.Multiplicity xmi.id="xmi.4">
                  <Foundation.Data_Types.Multiplicity.range>
                    <Foundation.Data_Types.MultiplicityRange xmi.id="xmi.5">
                      <Foundation.Data_Types.MultiplicityRange.lower>1</Foundation.Data_Types.MultiplicityRange.lower>
                      <Foundation.Data_Types.MultiplicityRange.upper>1</Foundation.Data_Types.MultiplicityRange.upper>
                    </Foundation.Data_Types.MultiplicityRange>
                  </Foundation.Data_Types.Multiplicity.range>
                </Foundation.Data_Types.Multiplicity>
              </Foundation.Core.StructuralFeature.multiplicity>
              <Foundation.Core.StructuralFeature.changeability xmi.value="changeable"/>
              <Foundation.Core.StructuralFeature.targetScope xmi.value="instance"/>
              <Foundation.Core.Attribute.initialValue>
                <Foundation.Data_Types.Expression xmi.id="xmi.6">
                  <Foundation.Data_Types.Expression.language>Java</Foundation.Data_Types.Expression.language>
                  <Foundation.Data_Types.Expression.body></Foundation.Data_Types.Expression.body>
                </Foundation.Data_Types.Expression>
              </Foundation.Core.Attribute.initialValue>
              <Foundation.Core.Feature.owner>
                <Foundation.Core.Classifier xmi.idref="xmi.2"/>
              </Foundation.Core.Feature.owner>
              <Foundation.Core.ModelElement.taggedValue>
                <Foundation.Extension_Mechanisms.TaggedValue xmi.id="xmi.7">
                  <Foundation.Extension_Mechanisms.TaggedValue.tag>transient</Foundation.Extension_Mechanisms.TaggedValue.tag>
                  <Foundation.Extension_Mechanisms.TaggedValue.value>false</Foundation.Extension_Mechanisms.TaggedValue.value>
                  <Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                    <Foundation.Core.ModelElement xmi.idref="xmi.3"/>
                  </Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                </Foundation.Extension_Mechanisms.TaggedValue>
                <Foundation.Extension_Mechanisms.TaggedValue xmi.id="xmi.8">
                  <Foundation.Extension_Mechanisms.TaggedValue.tag>volatile</Foundation.Extension_Mechanisms.TaggedValue.tag>
                  <Foundation.Extension_Mechanisms.TaggedValue.value>false</Foundation.Extension_Mechanisms.TaggedValue.value>
                  <Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                    <Foundation.Core.ModelElement xmi.idref="xmi.3"/>
                  </Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                </Foundation.Extension_Mechanisms.TaggedValue>
              </Foundation.Core.ModelElement.taggedValue>
            </Foundation.Core.Attribute>
            <Foundation.Core.Attribute xmi.id="xmi.9" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7ff0">
              <Foundation.Core.ModelElement.name>myDynamicAttribute</Foundation.Core.ModelElement.name>
              <Foundation.Core.ModelElement.visibility xmi.value="public"/>
              <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
              <Foundation.Core.Feature.ownerScope xmi.value="instance"/>
              <Foundation.Core.StructuralFeature.multiplicity>
                <Foundation.Data_Types.Multiplicity xmi.idref="xmi.4"/>
              </Foundation.Core.StructuralFeature.multiplicity>
              <Foundation.Core.StructuralFeature.changeability xmi.value="changeable"/>
              <Foundation.Core.StructuralFeature.targetScope xmi.value="instance"/>
              <Foundation.Core.Feature.owner>
                <Foundation.Core.Classifier xmi.idref="xmi.2"/>
              </Foundation.Core.Feature.owner>
              <Foundation.Core.ModelElement.taggedValue>
                <Foundation.Extension_Mechanisms.TaggedValue xmi.id="xmi.10">
                  <Foundation.Extension_Mechanisms.TaggedValue.tag>transient</Foundation.Extension_Mechanisms.TaggedValue.tag>
                  <Foundation.Extension_Mechanisms.TaggedValue.value>false</Foundation.Extension_Mechanisms.TaggedValue.value>
                  <Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                    <Foundation.Core.ModelElement xmi.idref="xmi.9"/>
                  </Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                </Foundation.Extension_Mechanisms.TaggedValue>
                <Foundation.Extension_Mechanisms.TaggedValue xmi.id="xmi.11">
                  <Foundation.Extension_Mechanisms.TaggedValue.tag>volatile</Foundation.Extension_Mechanisms.TaggedValue.tag>
                  <Foundation.Extension_Mechanisms.TaggedValue.value>false</Foundation.Extension_Mechanisms.TaggedValue.value>
                  <Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                    <Foundation.Core.ModelElement xmi.idref="xmi.9"/>
                  </Foundation.Extension_Mechanisms.TaggedValue.modelElement>
                </Foundation.Extension_Mechanisms.TaggedValue>
              </Foundation.Core.ModelElement.taggedValue>
            </Foundation.Core.Attribute>
            <Foundation.Core.Operation xmi.id="xmi.12" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7fee">
              <Foundation.Core.ModelElement.name>myMethod</Foundation.Core.ModelElement.name>
              <Foundation.Core.ModelElement.visibility xmi.value="public"/>
              <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
              <Foundation.Core.Feature.ownerScope xmi.value="instance"/>
              <Foundation.Core.BehavioralFeature.isQuery xmi.value="false"/>
              <Foundation.Core.Operation.concurrency xmi.value="sequential"/>
              <Foundation.Core.Operation.isRoot xmi.value="false"/>
              <Foundation.Core.Operation.isLeaf xmi.value="false"/>
              <Foundation.Core.Operation.isAbstract xmi.value="false"/>
              <Foundation.Core.Feature.owner>
                <Foundation.Core.Classifier xmi.idref="xmi.2"/>
              </Foundation.Core.Feature.owner>
              <Foundation.Core.BehavioralFeature.parameter>
                <Foundation.Core.Parameter xmi.id="xmi.13" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7fed">
                  <Foundation.Core.ModelElement.name>return</Foundation.Core.ModelElement.name>
                  <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
                  <Foundation.Core.Parameter.kind xmi.value="return"/>
                  <Foundation.Core.Parameter.behavioralFeature>
                    <Foundation.Core.BehavioralFeature xmi.idref="xmi.12"/>
                  </Foundation.Core.Parameter.behavioralFeature>
                  <Foundation.Core.Parameter.type>
                    <Foundation.Core.Classifier xmi.idref="xmi.14"/>
                  </Foundation.Core.Parameter.type>
                </Foundation.Core.Parameter>
                <Foundation.Core.Parameter xmi.id="xmi.15" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7fec">
                  <Foundation.Core.ModelElement.name>myArgument</Foundation.Core.ModelElement.name>
                  <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
                  <Foundation.Core.Parameter.kind xmi.value="in"/>
                  <Foundation.Core.Parameter.behavioralFeature>
                    <Foundation.Core.BehavioralFeature xmi.idref="xmi.12"/>
                  </Foundation.Core.Parameter.behavioralFeature>
                  <Foundation.Core.Parameter.type>
                    <Foundation.Core.Classifier xmi.idref="xmi.16"/>
                  </Foundation.Core.Parameter.type>
                </Foundation.Core.Parameter>
              </Foundation.Core.BehavioralFeature.parameter>
            </Foundation.Core.Operation>
          </Foundation.Core.Classifier.feature>
        </Foundation.Core.Class>
        <Foundation.Core.DataType xmi.id="xmi.14" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7fef">
          <Foundation.Core.ModelElement.name>void</Foundation.Core.ModelElement.name>
          <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isRoot xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isLeaf xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isAbstract xmi.value="false"/>
          <Foundation.Core.ModelElement.namespace>
            <Foundation.Core.Namespace xmi.idref="xmi.1"/>
          </Foundation.Core.ModelElement.namespace>
        </Foundation.Core.DataType>
        <Foundation.Core.DataType xmi.id="xmi.16" xmi.uuid="127-0-0-1-587175e0:105a31639e2:-7feb">
          <Foundation.Core.ModelElement.name>int</Foundation.Core.ModelElement.name>
          <Foundation.Core.ModelElement.isSpecification xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isRoot xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isLeaf xmi.value="false"/>
          <Foundation.Core.GeneralizableElement.isAbstract xmi.value="false"/>
          <Foundation.Core.ModelElement.namespace>
            <Foundation.Core.Namespace xmi.idref="xmi.1"/>
          </Foundation.Core.ModelElement.namespace>
        </Foundation.Core.DataType>
      </Foundation.Core.Namespace.ownedElement>
    </Model_Management.Model>
  </XMI.content>
</XMI>

XXX;
# ---------------------------------------------------------
$demo["r1format"] = "xmi";
$demo["r2format"] = "xmi";
savedemo();
# ---------------------------------------------------------

?>