import { Table as TableUI } from "@chakra-ui/react";

export const Table: React.FC = (props) => {
  return (
    <TableUI
      {...props}
      css={{ marginTop: 16, marginBottom: 16 }}
      variant="striped"
      border="1px solid"
      borderColor="gray.200"
    />
  );
};

export { Thead } from "@chakra-ui/react";
export { Tbody } from "@chakra-ui/react";
export { Tfoot } from "@chakra-ui/react";
export { Tr } from "@chakra-ui/react";
export { Th } from "@chakra-ui/react";
export { Td } from "@chakra-ui/react";
