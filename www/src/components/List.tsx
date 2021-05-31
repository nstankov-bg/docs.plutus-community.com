import { List as ChakraList } from "@chakra-ui/react";

export const List: React.FC = (props) => (
  <ChakraList
    {...props}
    css={{
      listStyleType: "circle",
      paddingLeft: 16,
      marginTop: 16,
      marginBottom: 16,
    }}
  />
);
