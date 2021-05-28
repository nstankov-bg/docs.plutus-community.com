import { useColorModeValue } from "@chakra-ui/color-mode";

export const P: React.FC = (props) => {
  const backgroundColor = useColorModeValue("#f5f7ff", "#282c34");
  const color = useColorModeValue("#282c34", "#abb2bf");

  return (
    <p
      {...props}
      css={{
        "& > code": {
          backgroundColor,
          padding: "2px 8px 2px 8px",
          borderRadius: 2,
          color,
        },
      }}
    />
  );
};
